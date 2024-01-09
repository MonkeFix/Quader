use argon2::{
    password_hash::{rand_core::OsRng, PasswordHash, PasswordHasher, PasswordVerifier, SaltString},
    Argon2,
};

const max_password_length: usize = 64;

pub fn hash(password: impl Into<String>) -> Result<String, crate::Error> {
    let password = password.into();

    if password.is_empty() {
        return Err(crate::Error::EmptyPassword);
    }

    if password.len() > max_password_length {
        return Err(crate::Error::ExceededMaxPasswordLength(max_password_length));
    }

    let salt = SaltString::generate(&mut OsRng);
    let hashed_password = Argon2::default()
        .hash_password(password.as_bytes(), &salt)
        .map_err(|_| crate::Error::HashingError)?
        .to_string();

    Ok(hashed_password)
}

pub fn compare(password: &str, hashed_password: &str) -> Result<bool, crate::Error> {
    if password.is_empty() {
        return Err(crate::Error::EmptyPassword);
    }

    if password.len() > max_password_length {
        return Err(crate::Error::ExceededMaxPasswordLength(max_password_length));
    }

    let parsed_hash =
        PasswordHash::new(hashed_password).map_err(|_| crate::Error::InvalidHashFormat)?;

    let password_matches = Argon2::default()
        .verify_password(password.as_bytes(), &parsed_hash)
        .map_or(false, |_| true);

    Ok(password_matches)
}
