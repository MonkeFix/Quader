using System.Runtime.InteropServices;

namespace ColdClearNet;

internal class ColdClearHandle : SafeHandle
{
    public override bool IsInvalid => handle == IntPtr.Zero;
    
    public ColdClearHandle() : base(IntPtr.Zero, true)
    { }

    protected override bool ReleaseHandle()
    {
        if (!IsInvalid)
            ColdClearInterop.DestroyAsync(handle);

        return true;
    }
}