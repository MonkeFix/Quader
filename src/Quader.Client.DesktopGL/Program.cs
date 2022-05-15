using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using Quader.Debugging.Logging;
using Quader.Debugging.Logging.Loggers;
using Thread = System.Threading.Thread;

namespace Quader
{
    public static class Program
    {
        public static string WorkingDirectory => AppDomain.CurrentDomain.BaseDirectory;

        [STAThread]
        static void Main()
        {
            LoggerFactory.DefaultLoggers = new List<ILoggerFrontend>
            {
                new ConsoleLogger(),
                new DiagnosticsLogger(),
                new FileLogger()
            };

            var logger = LoggerFactory.GetLogger("Main");

            AppDomain.CurrentDomain.UnhandledException += (sender, args) =>
            {
                var ex = args.ExceptionObject as Exception;
                logger.Critical(ex?.ToString() ?? "UNKNOWN EXCEPTION");
            };

            Directory.SetCurrentDirectory(WorkingDirectory);

            CultureInfo.DefaultThreadCurrentCulture = CultureInfo.InvariantCulture;
            Thread.CurrentThread.CurrentCulture = CultureInfo.InvariantCulture;
            Thread.CurrentThread.CurrentUICulture = CultureInfo.InvariantCulture;
            
            using var game = new GameRoot();
            game.Run();
        }
    }
}
