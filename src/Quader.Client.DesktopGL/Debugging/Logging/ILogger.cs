using System.Threading.Tasks;

namespace Quader.Debugging.Logging
{
    public interface ILogger
    {
        void Log(string message, LogLevel level);
        Task LogAsync(string message, LogLevel level);
    }
}