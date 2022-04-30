using System.Threading.Tasks;

namespace Quader.Debugging.Logging
{
    public interface ILoggerFrontend
    {
        void Log(string message, LogLevel level);
    }
}