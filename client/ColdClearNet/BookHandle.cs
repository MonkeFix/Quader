using System.Runtime.InteropServices;

namespace ColdClearNet;

public class BookHandle : SafeHandle
{
    public override bool IsInvalid => handle == IntPtr.Zero;
    
    public BookHandle() : base(IntPtr.Zero, true)
    { }

    protected override bool ReleaseHandle()
    {
        if (!IsInvalid)
            ColdClearInterop.DestroyBook(handle);

        return true;
    }
}