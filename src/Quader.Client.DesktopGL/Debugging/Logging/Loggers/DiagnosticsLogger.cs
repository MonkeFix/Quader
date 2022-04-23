using System.Threading.Tasks;

namespace Quader.Debugging.Logging.Loggers
{
    public class DiagnosticsLogger : LoggerBase
    {
        public override void Log(string message, LogLevel level)
        {
            System.Diagnostics.Debug.WriteLine(message);
        }

        public override Task LogAsync(string message, LogLevel level)
        {
            System.Diagnostics.Debug.WriteLine(message);

            return Task.CompletedTask;
        }
    }
}