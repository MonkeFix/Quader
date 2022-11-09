using System;
using System.Collections.Generic;
using System.Diagnostics;
using Quader.Debugging.Logging.Loggers;

namespace Quader.Debugging.Logging
{
    public static class LoggerFactory
    {
        private static readonly Dictionary<string, ILogger> Loggers = new ();

        public static List<ILoggerFrontend> DefaultLoggers { get; set; }

        static LoggerFactory()
        {
            DefaultLoggers = new List<ILoggerFrontend> { new ConsoleLogger(), new FileLogger(), new DiagnosticsLogger() };
        }


        public static ILogger GetLogger()
        {
            return GetLogger(DefaultLoggers);
        }

        public static ILogger GetLogger(List<ILoggerFrontend> loggers)
        {
            return GetLogger(new StackFrame(2).GetMethod()?.DeclaringType?.Name ?? "UNKNOWN", loggers);
        }

        public static ILogger GetLogger(Type context)
        {
            return GetLogger(context.Name, DefaultLoggers);
        }

        public static ILogger GetLogger(Type context, List<ILoggerFrontend> loggers)
        {
            return GetLogger(context.Name, loggers);
        }

        public static ILogger GetLogger(string context)
        {
            return GetLogger(context, DefaultLoggers);
        }

        public static ILogger GetLogger<T>()
        {
            return GetLogger(typeof(T).Name);
        }

        public static ILogger GetLogger(string context, List<ILoggerFrontend> loggers)
        {
            if (!Loggers.ContainsKey(context))
            {
                Loggers[context] = new Logger(context, loggers);
            }

            return Loggers[context];
        }
    }
}