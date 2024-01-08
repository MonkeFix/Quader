-- Add up migration script here

INSERT INTO users
       (username, email, password_hash, role) VALUES
       ('krendelhoff', 'savely@krendelhoff.space', '$argon2id$v=19$m=19456,t=2,p=1$GTK2jQ7jQdFldMBejjHPNA$+tQqMUFu7wVsB2KMtgwR7Kp2YrAbbAZElS/WdbwnSpo', 'admin');
