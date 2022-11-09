using System.Diagnostics;
using System.Threading.Tasks;

namespace Quader.Debugging.Logging;

public interface ILoggerFrontend
{
    [DebuggerHidden]
    void Log(string message, LogLevel level);
}