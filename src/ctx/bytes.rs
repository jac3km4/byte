use crate::{check_len, Error, Measure, Result, TryRead, TryWrite};

use super::{Len, Pattern, PatternUntil};

impl<'a> TryRead<'a, Len> for &'a [u8] {
    #[inline]
    fn try_read(bytes: &'a [u8], Len(len): Len) -> Result<(Self, usize)> {
        check_len(bytes, len)?;
        Ok((&bytes[..len], len))
    }
}

impl<'a> TryRead<'a, Pattern> for &'a [u8] {
    #[inline]
    fn try_read(bytes: &'a [u8], Pattern(pattern): Pattern) -> Result<(Self, usize)> {
        if pattern.is_empty() {
            return Err(Error::BadInput {
                err: "Pattern is empty",
            });
        }
        check_len(bytes, pattern.len())?;
        let len = (0..bytes.len() - pattern.len() + 1)
            .map(|n| bytes[n..].starts_with(pattern))
            .position(|p| p)
            .map(|len| len + pattern.len())
            .ok_or(Error::Incomplete)?;
        Ok((&bytes[..len], len))
    }
}

impl<'a> TryRead<'a, PatternUntil> for &'a [u8] {
    #[inline]
    fn try_read(
        bytes: &'a [u8],
        PatternUntil(pattern, len): PatternUntil,
    ) -> Result<(Self, usize)> {
        if pattern.is_empty() {
            return Err(Error::BadInput {
                err: "Pattern is empty",
            });
        }
        if pattern.len() > len {
            return Err(Error::BadInput {
                err: "Pattern is longer than restricted length",
            });
        }
        check_len(bytes, pattern.len())?;
        let len = (0..bytes.len() - pattern.len() + 1)
            .map(|n| bytes[n..].starts_with(pattern))
            .take(len - pattern.len())
            .position(|p| p)
            .map(|position| position + pattern.len())
            .unwrap_or(check_len(bytes, len)?);
        Ok((&bytes[..len], len))
    }
}

impl TryWrite for [u8] {
    #[inline]
    fn try_write(&self, bytes: &mut [u8], _ctx: ()) -> Result<usize> {
        check_len(bytes, self.len())?;
        bytes[..self.len()].copy_from_slice(self);
        Ok(self.len())
    }
}

impl TryWrite<Len> for [u8] {
    #[inline]
    fn try_write(&self, bytes: &mut [u8], Len(len): Len) -> Result<usize> {
        let len = len.min(self.len());
        check_len(bytes, len)?;
        bytes[..len].copy_from_slice(&self[..len]);
        Ok(len)
    }
}

impl TryWrite<Pattern> for [u8] {
    #[inline]
    fn try_write(&self, bytes: &mut [u8], _: Pattern) -> Result<usize> {
        self.try_write(bytes, ())
    }
}

impl TryWrite<PatternUntil> for [u8] {
    #[inline]
    fn try_write(&self, bytes: &mut [u8], PatternUntil(_, len): PatternUntil) -> Result<usize> {
        self.try_write(bytes, Len(len))
    }
}

impl Measure for [u8] {
    #[inline]
    fn measure(&self, _: ()) -> usize {
        self.len()
    }
}

impl Measure<Len> for [u8] {
    #[inline]
    fn measure(&self, Len(len): Len) -> usize {
        len.min(self.len())
    }
}

impl Measure<Pattern> for [u8] {
    #[inline]
    fn measure(&self, _: Pattern) -> usize {
        self.len()
    }
}

impl Measure<PatternUntil> for [u8] {
    #[inline]
    fn measure(&self, PatternUntil(_, len): PatternUntil) -> usize {
        len.min(self.len())
    }
}
