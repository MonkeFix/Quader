use chrono::{Duration, Utc};
use jsonwebtoken::{decode, encode, Algorithm, DecodingKey, EncodingKey, Header, Validation};
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

#[derive(Debug, Serialize, Deserialize, ToSchema)]
pub struct Claims {
    pub sub: String,
    pub iat: usize,
    pub exp: usize,
}

pub fn create_jwt(
    user_id: &str,
    secret: &[u8],
    expires_in_seconds: i64,
) -> Result<String, jsonwebtoken::errors::Error> {
    if user_id.is_empty() {
        return Err(jsonwebtoken::errors::ErrorKind::InvalidSubject.into());
    }

    let now = Utc::now();
    let iat = now.timestamp() as usize;
    let exp = (now + Duration::seconds(expires_in_seconds)).timestamp() as usize;
    let claims: Claims = Claims {
        sub: user_id.to_string(),
        exp,
        iat,
    };

    encode(
        &Header::default(),
        &claims,
        &EncodingKey::from_secret(secret),
    )
}

pub fn decode_jwt<T: Into<String>>(token: T, secret: &[u8]) -> Result<Claims, crate::Error> {
    let decoded = decode::<Claims>(
        &token.into(),
        &DecodingKey::from_secret(secret),
        &Validation::new(Algorithm::HS256),
    );
    match decoded {
        Ok(token) => Ok(token.claims),
        Err(_) => Err(crate::Error::InvalidToken),
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    #[test]
    fn test_empty_username_should_fail() {
        assert!(super::create_jwt("", b"", 60).is_err())
    }

    proptest! {
        #[test]
        fn test_encode_decode_roundtrip(s in prop::collection::vec(any::<char>(), 1..24)
                                        .prop_map(|v| v.into_iter().collect::<String>())
        ) {
            let secret = b"secret";
            let token = super::create_jwt(&s, secret, 60).expect("encode must succeed");
            let claims = super::decode_jwt(token, secret).expect("decode must succeed");
            assert_eq!(claims.sub, s)
        }
    }
}
