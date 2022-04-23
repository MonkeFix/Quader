namespace Quader.Debugging.Logging
{

    public static class LoggerExtensions
    {
        public static void Debug(this Logger logger, object message) =>
            logger.Log(message, LogLevel.Debug);

        public static void Info(this Logger logger, object message) =>
            logger.Log(message, LogLevel.Info);

        public static void Warn(this Logger logger, object message) =>
            logger.Log(message, LogLevel.Warning);

        public static void Critical(this Logger logger, object message) =>
            logger.Log(message, LogLevel.Critical);

        public static void Error(this Logger logger, object message) =>
            logger.Log(message, LogLevel.Error);

    }
}