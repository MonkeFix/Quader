using System;
using System.Collections.Generic;
using System.Diagnostics;
using Tetramonio.Debugging.Logging.Loggers;

namespace Tetramonio.Debugging.Logging
{
    public static class LoggerFactory
    {
        private static readonly Dictionary<string, Logger> LogHelpers = new Dictionary<string, Logger>();

        public static List<LoggerBase> DefaultLoggers { get; set; }

        static LoggerFactory()
        {
            DefaultLoggers = new List<LoggerBase> { new ConsoleLogger(), new FileLogger(), new DiagnosticsLogger() };
        }


        public static Logger GetLogger()
        {
            return GetLogger(DefaultLoggers);
        }

        public static Logger GetLogger(List<LoggerBase> loggers)
        {
            return GetLogger(new StackFrame(2).GetMethod()?.DeclaringType?.Name ?? "UNKNOWN", loggers);
        }

        public static Logger GetLogger(Type context)
        {
            return GetLogger(context.Name, DefaultLoggers);
        }

        public static Logger GetLogger(Type context, List<LoggerBase> loggers)
        {
            return GetLogger(context.Name, loggers);
        }

        public static Logger GetLogger(string context)
        {
            return GetLogger(context, DefaultLoggers);
        }

        public static Logger GetLogger(string context, List<LoggerBase> loggers)
        {
            if (!LogHelpers.ContainsKey(context))
            {
                LogHelpers[context] = new Logger(context, loggers);
            }

            return LogHelpers[context];
        }
    }
}