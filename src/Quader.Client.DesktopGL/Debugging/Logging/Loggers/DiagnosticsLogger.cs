using System.Threading.Tasks;

namespace Quader.Debugging.Logging.Loggers
{
    public class DiagnosticsLogger : ILogger
    {
        public void Log(object message, LogLevel level)
        {
            System.Diagnostics.Debug.WriteLine(message);
        }

        public Task LogAsync(object message, LogLevel level)
        {
            System.Diagnostics.Debug.WriteLine(message);

            return Task.CompletedTask;
        }
    }
}