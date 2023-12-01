namespace ColdClearNet;

public sealed class Book : IDisposable
{
    internal readonly BookHandle _book;

    public static Book Empty => new Book(new BookHandle());

    private Book(BookHandle book)
    {
        _book = book;
    }

    /// <summary>
    /// Loads an opening book from the specified file path.
    /// This supports both `.ccbook` and `.ccdb` books.
    /// If an error occurs, `NULL` is returned instead.
    /// </summary>
    /// <param name="path">File path</param>
    /// <returns>A book loaded from file</returns>
    public static Book? LoadFromFile(string path)
    {
        var ptr = ColdClearInterop.LoadBookFromFile(path);

        if (ptr.IsInvalid || ptr.IsClosed)
            return null;

        return new Book(ptr);
    }

    /// <summary>
    /// Loads an opening book from the specified book file contents.
    /// This only supports `.ccbook` books.
    /// If an error occurs, `null` is returned instead.
    /// </summary>
    /// <param name="data"></param>
    /// <returns></returns>
    public static Book? LoadFromMemory(byte[] data)
    {
        var ptr = ColdClearInterop.LoadBookFromMemory(data, (uint) data.Length);

        if (ptr.IsInvalid || ptr.IsClosed)
            return null;

        return new Book(ptr);
    }
    
    public void Dispose()
    {
        Dispose(true);
        GC.SuppressFinalize(this);
    }

    private void Dispose(bool disposing)
    {
        if (_book != null && !_book.IsInvalid)
            _book.Dispose();
    }

    ~Book()
    {
        Dispose(false);
    }
}