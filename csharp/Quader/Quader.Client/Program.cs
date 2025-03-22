namespace Quader.Client
{
    internal class Program
    {
        [STAThread]
        static void Main(string[] args)
        {
            using var game = new GameRoot();
            game.Run();
        }
    }
}