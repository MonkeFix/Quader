using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace Quader.Debugging.Logging
{
    /// <summary>
    /// Provides a logging system which might work on either sync or async.
    /// </summary>
    public class Logger
    {
        public string Context { get; }
        private List<LoggerBase> ActiveLoggers { get; }

        public void AddLogger(LoggerBase logger)
        {
            if (ActiveLoggers.Contains(logger))
            {
                return;
            }

            ActiveLoggers.Add(logger);
        }

        internal Logger(string context, IEnumerable<LoggerBase> activeLoggers)
        {
            Context = context;
            ActiveLoggers = new List<LoggerBase>();
            ActiveLoggers.AddRange(activeLoggers);
        }

        public void Log(object message, LogLevel level = LogLevel.Debug)
        {
            var builtString = BuildString(message, level);

            foreach (var logger in ActiveLoggers)
                logger.Log(builtString, level);
        }

        public async Task LogAsync(object message, LogLevel level = LogLevel.Debug)
        {
            var buildString = BuildString(message, level);

            foreach (var logger in ActiveLoggers)
                await logger.LogAsync(buildString, level).ConfigureAwait(false);
        }

        private string BuildString(object message, LogLevel level)
        {
            string result = "";
            string curDateTimeStr = DateTime.Now.ToString("G");

            switch (level)
            {
                case LogLevel.Debug:
                    result += $"[DEBUG] - {curDateTimeStr} - {Context} - ";
                    break;
                case LogLevel.Info:
                    result += $"[INFO]  - {curDateTimeStr} - {Context} - ";
                    break;
                case LogLevel.Warning:
                    result += $"[WARN]  - {curDateTimeStr} - {Context} - ";
                    break;
                case LogLevel.Error:
                    result += $"[ERROR] - {curDateTimeStr} - {Context} - ";
                    break;
                case LogLevel.Critical:
                    result += $"[FATAL] - {curDateTimeStr} - {Context} - ";
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(level), level, null);
            }
            
            return result + message.ToString();
        }
    }
}
