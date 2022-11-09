using System.Diagnostics;

namespace Quader.Debugging.Logging
{

    public static class LoggerExtensions
    {
        [DebuggerHidden]
        public static void Trace(this ILogger logger, object message) =>
            logger.Log(message, LogLevel.Trace);
        [DebuggerHidden]
        public static void Debug(this ILogger logger, object message) =>
            logger.Log(message, LogLevel.Debug);
        [DebuggerHidden]
        public static void Info(this ILogger logger, object message) =>
            logger.Log(message, LogLevel.Info);
        [DebuggerHidden]
        public static void Warn(this ILogger logger, object message) =>
            logger.Log(message, LogLevel.Warning);
        [DebuggerHidden]
        public static void Critical(this ILogger logger, object message) =>
            logger.Log(message, LogLevel.Critical);
        [DebuggerHidden]
        public static void Error(this ILogger logger, object message) =>
            logger.Log(message, LogLevel.Error);

    }
}