namespace ColdClearNet;

public sealed class Book : IDisposable
{
    internal IntPtr _book;

    private Book(IntPtr book)
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

        if (ptr == IntPtr.Zero)
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

        if (ptr == IntPtr.Zero)
            return null;

        return new Book(ptr);
    }


    private void ReleaseUnmanagedResources()
    {
        if (_book == IntPtr.Zero)
            return;

        ColdClearInterop.DestroyBook(_book);
        _book = IntPtr.Zero;
    }

    public void Dispose()
    {
        ReleaseUnmanagedResources();
        GC.SuppressFinalize(this);
    }

    ~Book()
    {
        ReleaseUnmanagedResources();
    }
}