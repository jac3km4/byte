use crate::{check_len, Error, Measure, Result, TryRead, TryWrite};
use core::str;

use super::{Delimiter, DelimiterUntil, Len};

/// Null string delimiter
pub const NULL: u8 = 0;
/// Space string delimiter
pub const SPACE: u8 = 0x20;
/// Return string delimiter
pub const RET: u8 = 0x0a;
/// Tab string delimiter
pub const TAB: u8 = 0x09;

impl<'a> TryRead<'a, Len> for &'a str {
    #[inline]
    fn try_read(bytes: &'a [u8], Len(len): Len) -> Result<(Self, usize)> {
        let (bytes, size) = {
            let len = check_len(bytes, len)?;
            (&bytes[..len], len)
        };

        match str::from_utf8(bytes) {
            Ok(str) => Ok((str, size)),
            Err(_) => Err(Error::BadInput { err: "UTF8 Error" }),
        }
    }
}

impl<'a> TryRead<'a, Delimiter> for &'a str {
    #[inline]
    fn try_read(bytes: &'a [u8], Delimiter(delimiter): Delimiter) -> Result<(Self, usize)> {
        let (bytes, size) = {
            let position = bytes
                .iter()
                .position(|c| *c == delimiter)
                .ok_or(Error::Incomplete)?;
            (&bytes[..position], position + 1)
        };

        match str::from_utf8(bytes) {
            Ok(str) => Ok((str, size)),
            Err(_) => Err(Error::BadInput { err: "UTF8 Error" }),
        }
    }
}

impl<'a> TryRead<'a, DelimiterUntil> for &'a str {
    #[inline]
    fn try_read(
        bytes: &'a [u8],
        DelimiterUntil(delimiter, len): DelimiterUntil,
    ) -> Result<(Self, usize)> {
        let (bytes, size) = {
            let position = bytes.iter().take(len).position(|c| *c == delimiter);
            match position {
                Some(position) => (&bytes[..position], position + 1),
                None => {
                    let len = check_len(bytes, len)?;
                    (&bytes[..len], len)
                }
            }
        };

        match str::from_utf8(bytes) {
            Ok(str) => Ok((str, size)),
            Err(_) => Err(Error::BadInput { err: "UTF8 Error" }),
        }
    }
}

impl TryWrite for str {
    #[inline]
    fn try_write(&self, bytes: &mut [u8], _: ()) -> Result<usize> {
        self.as_bytes().try_write(bytes, ())
    }
}

impl TryWrite<Len> for str {
    #[inline]
    fn try_write(&self, bytes: &mut [u8], len: Len) -> Result<usize> {
        self.as_bytes().try_write(bytes, len)
    }
}

impl TryWrite<Delimiter> for str {
    #[inline]
    fn try_write(&self, bytes: &mut [u8], Delimiter(delim): Delimiter) -> Result<usize> {
        let str_bytes = self.as_bytes();
        check_len(bytes, str_bytes.len() + 1)?;
        bytes[..str_bytes.len()].copy_from_slice(str_bytes);
        bytes[str_bytes.len()] = delim;
        Ok(str_bytes.len() + 1)
    }
}

impl Measure<()> for &str {
    #[inline]
    fn measure(&self, _: ()) -> usize {
        self.len()
    }
}

impl Measure<Delimiter> for &str {
    #[inline]
    fn measure(&self, _: Delimiter) -> usize {
        self.len() + 1
    }
}

impl Measure<Len> for &str {
    #[inline]
    fn measure(&self, Len(len): Len) -> usize {
        self.len().min(len)
    }
}

impl Measure<DelimiterUntil> for &str {
    #[inline]
    fn measure(&self, DelimiterUntil(_, len): DelimiterUntil) -> usize {
        (self.len() + 1).min(len)
    }
}
