#[cfg(test)]
mod tests {
    use quader_engine::add;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}