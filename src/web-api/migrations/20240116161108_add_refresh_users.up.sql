-- Add up migration script here

ALTER TABLE users ADD COLUMN refresh_token VARCHAR(255) NOT NULL DEFAULT '';
