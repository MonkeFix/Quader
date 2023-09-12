using System.Runtime.InteropServices;

namespace Quader.Engine.DllTests;

public static class QuaderEngineInterop
{
    public const string DllName = "Quader.Engine";
    
    [DllImport(DllName, EntryPoint = "q_board_create")]
    public static extern IntPtr CreateBoard();
    
    [DllImport(DllName, EntryPoint = "q_board_destroy")]
    public static extern void DestroyBoard(IntPtr board);
    
    
    [DllImport(DllName, EntryPoint = "q_test_create")]
    public static extern IntPtr CreateTest();
    
    [DllImport(DllName, EntryPoint = "q_test_destroy")]
    public static extern void DestroyTest(IntPtr test);
    
    [DllImport(DllName, EntryPoint = "q_test_test")]
    public static extern void TestTest(IntPtr test);
    
    [DllImport(DllName, EntryPoint = "q_test_get_points")]
    public static extern IntPtr GetPointsTest(IntPtr test);  

    [DllImport(DllName, EntryPoint = "write_line")]
    public static extern int WriteLine([MarshalAs(UnmanagedType.LPStr)]string pString);
}