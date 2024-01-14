use argon2::{
    password_hash::{rand_core::OsRng, PasswordHash, PasswordHasher, PasswordVerifier, SaltString},
    Argon2,
};

pub fn hash(password: impl Into<String>) -> Result<String, crate::Error> {
    let password = password.into();

    let salt = SaltString::generate(&mut OsRng);
    let hashed_password = Argon2::default()
        .hash_password(password.as_bytes(), &salt)
        .map_err(|_| crate::Error::HashingError)?
        .to_string();

    Ok(hashed_password)
}

pub fn compare(password: &str, hashed_password: &str) -> Result<bool, crate::Error> {
    let parsed_hash =
        PasswordHash::new(hashed_password).map_err(|_| crate::Error::InvalidHashFormat)?;

    let password_matches = Argon2::default()
        .verify_password(password.as_bytes(), &parsed_hash)
        .map_or(false, |_| true);

    Ok(password_matches)
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn test_random_strings(s in prop::collection::vec(any::<char>(), 1..128).prop_map(|v| v.into_iter().collect::<String>())) {
            assert!(super::compare(&s, &super::hash(&s).unwrap()).unwrap())
        }
    }
}
