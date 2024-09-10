use derive_more::Display;
use serde::Serialize;
use std::fmt;

#[cfg(feature = "full")]
use utoipa::ToSchema;

#[derive(Debug, Display, Copy, Clone, Serialize)]
#[cfg_attr(feature = "full", derive(ToSchema))]
#[serde(rename_all = "snake_case")]
pub enum Status {
    #[display("success")]
    Success,
    #[display("failure")]
    Failure,
    #[display("error")]
    Error,
}

fn use_display<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
where
    T: fmt::Display,
    S: serde::Serializer,
{
    serializer.collect_str(value)
}

#[derive(Debug, Serialize, Clone)]
#[cfg_attr(feature = "full", derive(ToSchema))]
pub struct Response {
    #[serde(serialize_with = "use_display")]
    pub status: Status,
    #[serde(serialize_with = "use_display")]
    pub message: Error,
}

impl fmt::Display for Response {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

#[derive(Debug, PartialEq, Display, Clone, Serialize)]
#[cfg_attr(feature = "full", derive(ToSchema))]
pub enum Error {
    #[display("Server Error. Please try again later")]
    ServerError,
    #[display("Email or password is wrong")]
    WrongCredentials,
    #[display("User with this email already exists")]
    EmailExist,
    #[display("User belonging to this token no longer exists")]
    UserNoLongerExist,
    #[display("User with this id does not exist")]
    UserDoesNotExist,
    #[display("Password cannot be empty")]
    EmptyPassword,
    #[display("Password must not be more than {} characters", _0)]
    ExceededMaxPasswordLength(usize),
    #[display("Error while hashing password")]
    HashingError,
    #[display("Invalid password hash format")]
    InvalidHashFormat,
    #[display("Authentication token is invalid")]
    InvalidToken,
    #[display("Refresh token is invalid")]
    InvalidRefreshToken,
    #[display("You are not logged in, please provide token")]
    TokenNotProvided,
    #[display("No refresh token provided")]
    RefreshTokenNotProvided,
    #[display("Access token is expired")]
    AccessTokenExpired,
    #[display("Refresh token is expired")]
    RefreshTokenExpired,
    #[display("You are not allowed to perform this action")]
    PermissionDenied,
    #[display("{}", _0)]
    Message(String),
}

impl Error {
    pub fn from_str(e: impl ToString) -> Self {
        Error::Message(e.to_string())
    }
}
