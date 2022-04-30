using System.Threading.Tasks;

namespace Quader.Debugging.Logging
{
    public interface ILogger
    {
        void Log(object message, LogLevel level);
        Task LogAsync(object message, LogLevel level);
    }
}