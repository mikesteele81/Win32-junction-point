{-# LANGUAGE ForeignFunctionInterface #-}

module System.Win32.JunctionPoint
    ( createJunctionPoint
    , deleteJunctionPoint
    , getJunctionPointInfo
    ) where

import Control.Exception (bracket)
import Data.Bits
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Foreign
import Foreign
import Foreign.C
import System.Win32 hiding (createFile)

-- Macro taken from winioctl.h
-- #define CTL_CODE( DeviceType, Function, Method, Access) ( \
--     (DWORD) ((DeviceType) << 16 | ((Access) << 14) | ((Function) << 2) \
--     | (Method))
cTL_CODE :: DWORD -> DWORD -> DWORD -> DWORD -> DWORD
cTL_CODE d f m a = d `shift` 16 .|. a `shift` 14 .|. f `shift` 2 .|. m

-- taken from winioctl.h
fILE_DEVICE_FILE_SYSTEM :: DWORD
fILE_DEVICE_FILE_SYSTEM = 0x00000009

-- taken from winioctl.h
mETHOD_BUFFERED :: DWORD
mETHOD_BUFFERED = 0

-- taken from winioctl.h
fILE_ANY_ACCESS, fILE_SPECIAL_ACCESS :: DWORD
fILE_ANY_ACCESS     = 0
fILE_SPECIAL_ACCESS = 0

-- taken from winioctl.h
fSCTL_SET_REPARSE_POINT :: DWORD
fSCTL_SET_REPARSE_POINT = cTL_CODE fILE_DEVICE_FILE_SYSTEM 41
    mETHOD_BUFFERED fILE_SPECIAL_ACCESS
fSCTL_GET_REPARSE_POINT :: DWORD
fSCTL_GET_REPARSE_POINT = cTL_CODE fILE_DEVICE_FILE_SYSTEM 42
    mETHOD_BUFFERED fILE_ANY_ACCESS
fSCTL_DELETE_REPARSE_POINT :: DWORD
fSCTL_DELETE_REPARSE_POINT = cTL_CODE fILE_DEVICE_FILE_SYSTEM 43
    mETHOD_BUFFERED fILE_SPECIAL_ACCESS

iO_REPARSE_TAG_MOUNT_POINT :: DWORD
iO_REPARSE_TAG_MOUNT_POINT = 0xA0000003

--taken from MSDN's OpenFileById documentation
fILE_FLAG_OPEN_REPARSE_POINT :: DWORD
fILE_FLAG_OPEN_REPARSE_POINT = 0x00200000

mAXIMUM_REPARSE_DATA_BUFFER_SIZE :: Int
mAXIMUM_REPARSE_DATA_BUFFER_SIZE = 16 * 1024

-- The first 3 fields of TMN_REPARSE_DATA_BUFFER are considered the
-- header. This is used in setReparsePoint.
tMN_REPARSE_DATA_BUFFER_HEADER_SIZE :: DWORD
tMN_REPARSE_DATA_BUFFER_HEADER_SIZE = 8

-- C structure. This is not documented in current versions of the
-- Windows SDK.

-- struct TMN_REPARSE_DATA_BUFFER
-- {
--     DWORD  ReparseTag;
--     WORD   ReparseDataLength;
--     WORD   Reserved;
--     WORD   SubstituteNameOffset;
--     WORD   SubstituteNameLength;
--     WORD   PrintNameOffset;
--     WORD   PrintNameLength;
--     WCHAR  PathBuffer[1];
-- };

data TMN_REPARSE_DATA_BUFFER = TMN_REPARSE_DATA_BUFFER
    { _reparseTag           :: !DWORD
    -- Bytes in addition to first 8. This will be 12 + length of _pathBuffer
    , _reparseDataLength    :: !WORD
    , _reserved             :: !WORD
    , _substituteNameOffset :: !WORD
    , _substituteNameLength :: !WORD
    , _printNameOffset      :: !WORD
    , _printNameLength      :: !WORD
    -- The C struct indicates a single-element array.
    -- Actually, we need to allocate enough memory to
    -- hold a string here.
    , _pathBuffer :: !(Ptr CWchar)
    }

instance Storable TMN_REPARSE_DATA_BUFFER where
  -- Storable does not allow the pokes of different sizes, so we're just
  -- allocating the maximum size Microsoft documents.
  sizeOf _    = mAXIMUM_REPARSE_DATA_BUFFER_SIZE
  alignment _ = 1 -- no alignment? I don't know what this means.
  peek ptr    = do
    reparseTag           <- peek . castPtr $ ptr
    reparseDataLength    <- castPtr ptr `peekByteOff` 4
    reserved             <- castPtr ptr `peekByteOff` 6
    substituteNameOffset <- castPtr ptr `peekByteOff` 8
    substituteNameLength <- castPtr ptr `peekByteOff` 10
    printNameOffset      <- castPtr ptr `peekByteOff` 12
    printNameLength      <- castPtr ptr `peekByteOff` 14
    let pathBuffer        = castPtr ptr `plusPtr`     16
    return $ TMN_REPARSE_DATA_BUFFER reparseTag reparseDataLength reserved
           substituteNameOffset substituteNameLength printNameOffset
           printNameLength pathBuffer
  poke ptr rdb = do
    castPtr ptr `poke` _reparseTag rdb
    castPtr ptr `pokeByteOff` 4  $ _reparseDataLength rdb
    castPtr ptr `pokeByteOff` 6  $ _reserved rdb
    castPtr ptr `pokeByteOff` 8  $ _substituteNameOffset rdb
    castPtr ptr `pokeByteOff` 10 $ _substituteNameLength rdb
    castPtr ptr `pokeByteOff` 12 $ _printNameOffset rdb
    castPtr ptr `pokeByteOff` 14 $ _printNameLength rdb
    strLen <- lengthArray0 0 $ _pathBuffer rdb
    copyArray (ptr `plusPtr` 16) (_pathBuffer rdb) (strLen + 1)

-- This data structure is almost identical to TMN_REPARSE_DATA_BUFFER, but the
-- meaning of fields differ. Microsoft documents this structure. It is
-- used for querying existing reparse points.
data REPARSE_GUID_DATA_BUFFER = REPARSE_GUID_DATA_BUFFER
    { _rgdb_ReparseTag :: DWORD
    -- Bytes used by _rgdb_DataBuffer. Contrast this with TMN_REPARSE_DATA_BUFFER.
    , _rgdb_ReparseDataLength :: !WORD
    , _rgdb_Reserved          :: !WORD
    -- MS specifies an odd structure, but a GUID is really just a 128-bit
    -- value.
    , _rgdb_GUID1             :: !DWORD
    , _rgdb_GUID2             :: !DWORD
    , _rgdb_GUID3             :: !DWORD
    , _rgdb_GUID4             :: !DWORD
    -- The C struct indicates a single-element array.
    -- Actually, we need to allocate enough memory to
    -- hold a string here.
    , _rgdb_DataBuffer :: !(Ptr BYTE)
    }

instance Storable REPARSE_GUID_DATA_BUFFER where
  -- Storable does not allow the pokes of different sizes, so we're just
  -- allocating the maximum size Microsoft documents.
  sizeOf _ = mAXIMUM_REPARSE_DATA_BUFFER_SIZE
  alignment _ = 1 -- no alignment? I don't know what this means.
  peek ptr = do
    _rgdb_ReparseTag        <- peek . castPtr $ ptr
    _rgdb_ReparseDataLength <- castPtr ptr `peekByteOff` 4
    _rgdb_Reserved          <- castPtr ptr `peekByteOff` 6
    _rgdb_GUID1             <- castPtr ptr `peekByteOff` 8
    _rgdb_GUID2             <- castPtr ptr `peekByteOff` 12
    _rgdb_GUID3             <- castPtr ptr `peekByteOff` 16
    _rgdb_GUID4             <- castPtr ptr `peekByteOff` 20
    let dataBuffer           = castPtr ptr `plusPtr`     24
    return $ REPARSE_GUID_DATA_BUFFER _rgdb_ReparseTag
           _rgdb_ReparseDataLength _rgdb_Reserved _rgdb_GUID1 _rgdb_GUID2
           _rgdb_GUID3 _rgdb_GUID4 dataBuffer
  poke ptr rdb = do
    castPtr ptr `poke` _rgdb_ReparseTag rdb
    castPtr ptr `pokeByteOff` 4  $ _rgdb_ReparseDataLength rdb
    castPtr ptr `pokeByteOff` 6  $ _rgdb_Reserved rdb
    castPtr ptr `pokeByteOff` 8  $ _rgdb_GUID1 rdb
    castPtr ptr `pokeByteOff` 12 $ _rgdb_GUID2 rdb
    castPtr ptr `pokeByteOff` 16 $ _rgdb_GUID3 rdb
    castPtr ptr `pokeByteOff` 20 $ _rgdb_GUID4 rdb
    buffer <- peekArray bufferSize (_rgdb_DataBuffer rdb)
    plusPtr ptr 24 `pokeArray` buffer
    where
      bufferSize = fromIntegral $ _rgdb_ReparseDataLength rdb - 24

withTMN_REPARSE_DATA_BUFFER :: Text
    -> (Ptr TMN_REPARSE_DATA_BUFFER -> IO a) -> IO a
withTMN_REPARSE_DATA_BUFFER dst f =
    useAsPtr0 dst $ \c_dst ->
    with (TMN_REPARSE_DATA_BUFFER
          { _reparseTag           = iO_REPARSE_TAG_MOUNT_POINT
          , _reparseDataLength    = dstLen + 12
          , _reserved             = 0
          , _substituteNameOffset = 0
          , _substituteNameLength = dstLen
          , _printNameOffset      = dstLen + 2
          , _printNameLength      = 0
          , _pathBuffer           = c_dst
          }) f
  where
    dstLen = fromIntegral (T.length dst) * 2

withREPARSE_GUID_DATA_BUFFER :: [BYTE]
    -> (Ptr REPARSE_GUID_DATA_BUFFER -> IO a) -> IO a
withREPARSE_GUID_DATA_BUFFER bx f =
    withArray bx $ \dataBuffer ->
    with (REPARSE_GUID_DATA_BUFFER
          { _rgdb_ReparseTag = iO_REPARSE_TAG_MOUNT_POINT
          -- must be 0 when deleting a junction point
          , _rgdb_ReparseDataLength = 0
          , _rgdb_Reserved = 0
          , _rgdb_GUID1 = 0
          , _rgdb_GUID2 = 0
          , _rgdb_GUID3 = 0
          , _rgdb_GUID4 = 0
          , _rgdb_DataBuffer = dataBuffer
          }) f

-- | Create a junction point between two folders on the same filesystem.
--
-- 'mountDir' should be the full file path to an empty folder. This
-- folder should be on a local filesystem.
--
-- 'destDir' should be the full file path to the junction point's target. The
-- target must be a folder on the same filesystem as its source. The path
-- should also be preceeded by \"\\??\\\". This indicates to Windows not to try
-- validating the path before using it.
--
-- >>> createJunctionPoint "c:\\Windows\\System32" "\\??\\c:\\System32"
createJunctionPoint :: Text -- ^ mountDir
                    -> Text -- ^ destDir
                    -> IO ()
createJunctionPoint mountDir destDir =
    withTMN_REPARSE_DATA_BUFFER destDir $ \rdb ->
    bracket (openReparseHandle mountDir) closeHandle $ \handle ->
    setReparsePoint handle rdb

-- | This "Deletes" the junction point at the supplied path. On success an
-- empty folder will be left in its place.
deleteJunctionPoint :: Text -> IO ()
deleteJunctionPoint dir =
    bracket (openReparseHandle dir) closeHandle $ \handle -> do
        deleteReparsePoint handle

-- | Discover the target of a junction point at the supplied path. An
-- exception will be raised if the target is either invalid or not a junction
-- point. The returned path will be prefixed by \"\\??\\\".
getJunctionPointInfo :: Text -> IO Text
getJunctionPointInfo dir =
    bracket (openReparseHandle dir) closeHandle $ \handle ->
    with (0 :: DWORD) $ \bytesReturned ->
    withTMN_REPARSE_DATA_BUFFER (T.pack "") $ \pRdb -> do
        deviceIoControl handle fSCTL_GET_REPARSE_POINT Nothing 0
            (Just $ castPtr pRdb)
            (fromIntegral mAXIMUM_REPARSE_DATA_BUFFER_SIZE)
            (Just bytesReturned) Nothing
        --not sure how to call IsReparseTagValid.
        rdb <- peek pRdb
        fromPtr0 $ (_pathBuffer rdb)

-- NTFS junction points are implemented as reparse points. Reparse points are
-- extra tagged information attached to filesystem objects. It is up to
-- application software such as the Windows Shell to detect the presence of
-- reparse points and act approprietly.
-- 
-- There are many types of reparse points. For the internal purposes of
-- this library I am pretending that junction points are the only kind of
-- reparse point.
setReparsePoint :: HANDLE -> Ptr TMN_REPARSE_DATA_BUFFER -> IO ()
setReparsePoint handle pRdb =
    -- we don't care about bytesReturned
    with (0 :: DWORD) $ \bytesReturned -> do
        rdb <- peek pRdb
        deviceIoControl handle fSCTL_SET_REPARSE_POINT
            (Just $ castPtr pRdb)
            (tMN_REPARSE_DATA_BUFFER_HEADER_SIZE +
                (fromIntegral $ _reparseDataLength rdb))
            Nothing 0 (Just bytesReturned) Nothing

-- 'deleteReparsePoint' will remove a reparse point pointed to by the
-- argument. An exception will be raised if the argument does not point to an
-- open reparse point, or possibly if the user does not have write attribute
-- permissions to the object.
deleteReparsePoint :: HANDLE -> IO ()
deleteReparsePoint handle =
    -- When deleting reparse points the destination does not matter.
    -- msdn docs refer to REPARSE_GUID_DATA_BUFFER for this instead.
    withREPARSE_GUID_DATA_BUFFER [] $ \pRgdb ->
    with (0 :: DWORD) $ \bytesReturned -> do
        deviceIoControl handle fSCTL_DELETE_REPARSE_POINT
            (Just $ castPtr pRgdb)
            tMN_REPARSE_DATA_BUFFER_HEADER_SIZE
            Nothing 0 (Just bytesReturned) Nothing

-- Open a reparse point attached to the supplied folder. An exception will be
-- raised if the target does not exist, the user does not have read
-- permissions to it, or the target does not have a reparse point attached.
openReparseHandle :: Text -> IO HANDLE
openReparseHandle path = createFile path (gENERIC_READ .|. gENERIC_WRITE)
    fILE_SHARE_NONE Nothing oPEN_EXISTING
    (fILE_FLAG_BACKUP_SEMANTICS .|. fILE_FLAG_OPEN_REPARSE_POINT)
    Nothing

-- Low-level filesystem manipulation function. Check documentation in the MSDN
-- library.
deviceIoControl :: HANDLE -> DWORD -> Maybe LPVOID -> DWORD -> Maybe LPVOID
    -> DWORD -> Maybe LPDWORD -> Maybe LPOVERLAPPED -> IO ()
deviceIoControl hDevice dwIoControlCode lpInBuffer nInBufferSize
    lpOutBuffer nOutBufferSize lpBytesReturned lpOverlapped =
    failIfFalse_ (unwords [ "DeviceIoControl", show hDevice
                          , show dwIoControlCode]) $
        c_DeviceIoControl hDevice dwIoControlCode
            (maybe nullPtr id lpInBuffer) nInBufferSize
            (maybe nullPtr id lpOutBuffer) nOutBufferSize
            (maybe nullPtr id lpBytesReturned)
            (maybe nullPtr id lpOverlapped)

foreign import stdcall unsafe "windows.h DeviceIoControl"
    c_DeviceIoControl :: HANDLE -> DWORD -> LPVOID -> DWORD -> LPVOID
        -> DWORD -> LPDWORD -> LPOVERLAPPED -> IO Bool

createFile :: Text -> AccessMode -> ShareMode -> Maybe LPSECURITY_ATTRIBUTES
    -> CreateMode -> FileAttributeOrFlag -> Maybe HANDLE -> IO HANDLE
createFile name access share mb_attr mode flag mb_h =
    -- simply converting Text to a name does not add a null character
    useAsPtr0 name $ \ c_name ->
    failIf (==iNVALID_HANDLE_VALUE) (unwords ["CreateFile", show name]) $
    c_CreateFile c_name access share (maybePtr mb_attr) mode flag (maybePtr mb_h)

-- | useAsPtr returns a length and byte buffer, but all the win32 functions
-- rely on null termination.
useAsPtr0 :: Text -> (Ptr CWchar -> IO a) -> IO a
useAsPtr0 t f = useAsPtr (T.snoc t (chr 0x0)) $ \ str _ -> f  (castPtr str)

-- This traverses the string twice. Is there a faster way?
fromPtr0 :: Ptr CWchar -> IO Text
fromPtr0 ptr = do
    -- length in 16-bit words.
    len <- lengthArray0 0x0000 ptr'
    -- no loss of precision here. I16 is a newtype wrapper around Int.
    fromPtr ptr' $ fromIntegral len
  where
    ptr' :: Ptr Word16
    ptr' = castPtr ptr
