namespace Quader.Engine.DllTests;

class Program
{
    public static void Main(string[] args)
    {
        Console.WriteLine("Hello World!");
        QuaderEngineInterop.WriteLine("Hello!");
        //var boardPtr = QuaderEngineInterop.CreateBoard();
        //QuaderEngineInterop.DestroyBoard(boardPtr);
        var testPtr = QuaderEngineInterop.CreateTest();
        QuaderEngineInterop.TestTest(testPtr);
        var pts = QuaderEngineInterop.GetPointsTest(testPtr);
        QuaderEngineInterop.DestroyTest(testPtr);
    }
}