using System.Threading.Tasks;

namespace Quader.Debugging.Logging
{
    public interface ILogger
    {
        bool IsEnabled(LogLevel level);

        void Log(object message, LogLevel level);
    }

    public interface ILogger<out T> : ILogger
    { }
}