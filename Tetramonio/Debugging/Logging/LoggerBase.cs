using System.Threading.Tasks;

namespace Tetramonio.Debugging.Logging
{
    public abstract class LoggerBase
    {
        protected readonly object LockObject = new object();

        public abstract void Log(string message, LogLevel level);
        public abstract Task LogAsync(string message, LogLevel level);
    }
}
