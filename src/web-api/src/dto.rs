use serde::{Deserialize, Serialize};
use utoipa::ToSchema;
use validator::Validate;

#[derive(Validate, Debug, Default, Clone, Deserialize, ToSchema)]
pub struct RegisterUser {
    #[validate(
        length(min = 1, message = "Username is required"),
        length(max = 24, message = "Username is too long")
    )]
    pub username: String,
    #[validate(
        length(min = 1, message = "Email is required"),
        email(message = "Email is invalid")
    )]
    pub email: String,
    #[validate(
        length(min = 1, message = "Password is required"),
        length(min = 6, message = "Password must be at least 6 characters"),
        length(max = 64, message = "Password must not exceed 64 characters")
    )]
    pub password: String,
    #[validate(
        length(min = 1, message = "Please confirm your password"),
        must_match(other = "password", message = "Passwords do not match")
    )]
    #[serde(rename = "passwordConfirm")]
    pub password_confirm: String,
}

#[derive(Validate, Debug, Default, Clone, Deserialize, ToSchema)]
pub struct LoginUser {
    #[validate(
        length(min = 1, message = "Email is required"),
        email(message = "Email is invalid")
    )]
    pub email: String,
    #[validate(
        length(min = 1, message = "Password is required"),
        length(min = 6, message = "Password must be at least 6 characters"),
        length(max = 64, message = "Password must not exceed 64 characters")
    )]
    pub password: String,
}

#[derive(Debug, Serialize, ToSchema)]
pub struct TokenData {
    pub token: String,
    pub refresh_token: String,
}
