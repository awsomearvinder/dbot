#![allow(unused_variables)]
use serde::{
    de::{self, SeqAccess, Visitor},
    Deserialize,
};
use unicode_segmentation::UnicodeSegmentation;
pub struct Deserializer<'de> {
    // We truncate off part of the string as it's serialized to another form, when there's nothing left we're done.
    // Commands *shouldn't* require look back to deserialize, that is a preceeding token shouldn't impact what the next
    // token is serialized as.
    // note: We truncate starting from the back. The reason being strings will always be starting from the front, and they can be
    // as long as you want, and can include types like integers and the like.
    input: &'de str,
}
impl<'a> From<&'a str> for Deserializer<'a> {
    fn from(s: &'a str) -> Self {
        Self { input: s }
    }
}
//TODO: Implement these
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Error<'a> {
    EndedInQuotes,
    TrailingCharacters { trailing: &'a str },
    InvalidType,
}

impl std::fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error")
    }
}
impl std::error::Error for Error<'_> {}
impl serde::de::Error for Error<'_> {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        eprintln!("{}", msg);
        Error::InvalidType
    }
}
pub fn from_str<'a, T>(s: &'a str) -> Result<T, Error>
where
    T: Deserialize<'a>,
{
    let mut d = Deserializer::from(s);
    let t = T::deserialize(&mut d)?;
    if d.input.is_empty() {
        Ok(t)
    } else {
        Err(Error::TrailingCharacters { trailing: d.input })
    }
}
macro_rules! impl_deserialize_int {
    ($method_name: ident, $visitor_method_name: ident) => {
        impl_deserialize_num! {$method_name, $visitor_method_name, |c| {
            char::is_ascii_digit(c) || *c == '-'
        }}
    };
}
macro_rules! impl_deserialize_float {
    ($method_name: ident, $visitor_method_name: ident) => {
        impl_deserialize_num! {$method_name, $visitor_method_name, |c| {
            char::is_ascii_digit(c) || *c == '-' || *c == '.'
        }}
    };
}
macro_rules! impl_deserialize_num {
    ($method_name: ident, $visitor_method_name: ident, $num_can_contain: expr) => {
        fn $method_name<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: de::Visitor<'de>,
        {
            self.input = self.input.trim();
            let numeric_digits = self.input.chars().take_while($num_can_contain);
            let index_of_last_char = numeric_digits.enumerate().map(|(i, c)| i).last();
            if let Some(i) = index_of_last_char {
                let int = self
                    .input
                    .get(0..i + 1)
                    .ok_or(Error::InvalidType) // There shouldn't be any code path that gets here, but if there is,
                    // we do *not* want to panic, so better to just return an error.
                    .and_then(|s| s.parse().map_err(|_| Error::InvalidType))?;
                let out = visitor.$visitor_method_name(int);
                // If i + 1 is out of bounds, the remaining string must be empty logically.
                self.input = &self.input.get(i + 1..).unwrap_or("");
                out
            } else {
                Err(Error::InvalidType)
            }
        }
    };
}
impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de>
where
    'de: 'a,
{
    type Error = Error<'de>;
    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        use std::ops::ControlFlow;
        self.input = self.input.trim();
        let mut graphemes = self.input.grapheme_indices(true);
        let s = graphemes.try_fold((String::new(), false, 0), |(acc, escaped, _), (i, next)| {
            if escaped {
                return ControlFlow::Continue((acc + next, false, i + 1));
            }
            if next == "\\" {
                return ControlFlow::Continue((acc, true, i + 1));
            }
            if next == "\"" && acc.is_empty() {
                return ControlFlow::Continue((acc, false, i + 1));
            }
            if next == "\"" {
                return ControlFlow::Break((acc, i + 1));
            }
            ControlFlow::Continue((acc + next, false, i + 1))
        });

        let (s, byte_offset) = match s {
            ControlFlow::Continue((_, true, _)) => return Err(Error::EndedInQuotes),
            ControlFlow::Continue((acc, _, byte_offset)) => (acc, byte_offset),
            ControlFlow::Break((acc, byte_offset)) => (acc, byte_offset),
        };
        // Is there a way to get rid of this index?
        // Changing the code above might cause a panic currently if byte_offset >= len (although we know that it won't ever be that.)
        self.input = &self.input[byte_offset..];
        visitor.visit_string(s)
    }

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        if let Some(v) = self.input.trim().strip_prefix("true") {
            self.input = v;
            visitor.visit_bool(true)
        } else if let Some(v) = self.input.trim().strip_prefix("false") {
            self.input = v;
            visitor.visit_bool(false)
        } else {
            Err(Error::InvalidType)
        }
    }
    impl_deserialize_int! {deserialize_i8, visit_i8}
    impl_deserialize_int! {deserialize_i16, visit_i16}
    impl_deserialize_int! {deserialize_i32, visit_i32}
    impl_deserialize_int! {deserialize_i64, visit_i64}
    impl_deserialize_int! {deserialize_i128, visit_i128}
    impl_deserialize_int! {deserialize_u8, visit_u8}
    impl_deserialize_int! {deserialize_u16, visit_u16}
    impl_deserialize_int! {deserialize_u32, visit_u32}
    impl_deserialize_int! {deserialize_u64, visit_u64}
    impl_deserialize_int! {deserialize_u128, visit_u128}
    //note: we don't accept infinity, negative infinity, NaN, or any of those shennanigans.
    impl_deserialize_float! {deserialize_f32, visit_f32}
    impl_deserialize_float! {deserialize_f64, visit_f64}

    //NOTE: THIS MAY NOT BE WHAT YOU WANT. THIS SERIALIZES A CHAR NOT A GRAPHEME.
    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.input = self.input.trim();
        if let Some(c) = self.input.chars().next() {
            self.input = &self.input[char::len_utf8(c)..];
            visitor.visit_char(c)
        } else {
            Err(Error::InvalidType)
        }
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_seq(CommandList::new(self))
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }
}
struct CommandList<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}
impl<'a, 'de: 'a> CommandList<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Self { de }
    }
}
impl<'a, 'de: 'a> SeqAccess<'de> for CommandList<'a, 'de> {
    type Error = Error<'de>;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(&mut *self.de).map(Some)
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;
    #[test]
    fn string_impl_no_quotes() {
        assert_eq!(
            from_str("test one two three"),
            Ok(String::from("test one two three"))
        )
    }
    #[test]
    fn string_impl_quotes() {
        assert_eq!(
            from_str::<String>("\"test one two three\""),
            Ok(String::from("test one two three"))
        )
    }
    #[test]
    fn char_impl() {
        assert_eq!(from_str::<char>("c"), Ok('c'))
    }
    #[test]
    fn char_impl_then_string() {
        assert_eq!(
            from_str::<(char, String)>("c woah"),
            Ok(('c', String::from("woah")))
        )
    }
    #[test]
    fn string_impl_quotes_trailing() {
        assert_eq!(
            from_str::<String>("\"test one two three\" a"),
            Err(Error::TrailingCharacters { trailing: " a" })
        )
    }
    #[test]
    fn quote_string_empty_string_tuple() {
        assert_eq!(
            from_str::<(String, String)>("\"test one two three\""),
            Ok((String::from("test one two three"), String::from("")))
        )
    }
    #[derive(Deserialize, Debug, PartialEq, Eq)]
    struct B(i32, String);
    #[test]
    fn test_tuple_struct() {
        assert_eq!(
            from_str::<B>("5 Test one two three"),
            Ok(B(5, String::from("Test one two three")))
        )
    }
    #[derive(Deserialize, Debug, PartialEq, Eq)]
    struct C(String, i32);
    #[test]
    fn test_tuple_struct_rev() {
        assert_eq!(
            from_str::<C>("\"test one two\" 3"),
            Ok(C(String::from("test one two"), 3))
        )
    }
    #[test]
    fn quote_string_followed_by_unquoted() {
        assert_eq!(
            from_str::<(String, String)>("\"test one two three\" a"),
            Ok((String::from("test one two three"), String::from("a")))
        )
    }
    // It'd be nice to make this work one day, but it dosen't right now. :(
    #[test]
    fn test_string_no_quote_followed_by_bool() {
        assert_eq!(
            from_str::<(String, bool)>("yay one two true"),
            Err(Error::InvalidType)
        )
    }
    // this works though.
    #[test]
    fn test_bool_followed_by_string() {
        assert_eq!(
            from_str::<(bool, String)>("true yay one two"),
            Ok((true, String::from("yay one two")))
        )
    }
    #[test]
    fn bool() {
        assert_eq!(from_str::<bool>("true"), Ok(true))
    }
    #[test]
    fn quote_string_followed_by_bool() {
        assert_eq!(
            from_str::<(String, bool)>("\"test one two false\" false"),
            Ok((String::from("test one two false"), false))
        )
    }
    #[test]
    fn i8() {
        assert_eq!(from_str::<i8>("5"), Ok(5))
    }
    #[test]
    fn empty_i8() {
        assert_eq!(from_str::<i8>(""), Err(Error::InvalidType))
    }
    #[test]
    fn overflowing_i8() {
        assert_eq!(
            from_str::<i8>(&format!("{}", i32::MAX)),
            Err(Error::InvalidType)
        )
    }
    #[test]
    fn negative_i8() {
        assert_eq!(from_str::<i8>(&format!("{}", i8::MIN)), Ok(i8::MIN))
    }
    #[test]
    fn negative_u8() {
        assert_eq!(
            from_str::<u8>(&format!("{}", i8::MIN)),
            Err(Error::InvalidType)
        )
    }
    #[test]
    fn i8_then_u8() {
        assert_eq!(
            from_str::<(i8, u8)>(&format!("{} {}", i8::MIN, u8::MAX)),
            Ok((i8::MIN, u8::MAX))
        )
    }
    #[test]
    fn f32() {
        assert_eq!(from_str::<f32>("0.53"), Ok(0.53))
    }
    #[test]
    fn f32_whole() {
        assert_eq!(from_str::<f32>("4"), Ok(4.0))
    }
    #[derive(Deserialize, Debug, PartialEq, Eq)]
    struct A;
    #[test]
    fn test_unit_struct() {
        assert_eq!(from_str::<A>(""), Ok(A))
    }
    #[test]
    fn test_unit() {
        assert_eq!(from_str::<()>(""), Ok(()))
    }
}
