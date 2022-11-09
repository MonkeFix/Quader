using System.Diagnostics;
using System.Threading.Tasks;

namespace Quader.Debugging.Logging.Loggers;

public class DiagnosticsLogger : ILoggerFrontend
{
    [DebuggerHidden]
    public void Log(string message, LogLevel level)
    {
        Debug.WriteLine(message);
    }
}