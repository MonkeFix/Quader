using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Reflection;
using Quader.Debugging.Logging;
using Quader.Debugging.Logging.Loggers;
using Thread = System.Threading.Thread;

namespace Quader;

public static class Program
{
    public static string WorkingDirectory => AppDomain.CurrentDomain.BaseDirectory;

    [STAThread]
    static void Main()
    {
        Directory.SetCurrentDirectory(WorkingDirectory);

        CultureInfo.DefaultThreadCurrentCulture = CultureInfo.InvariantCulture;
        Thread.CurrentThread.CurrentCulture = CultureInfo.InvariantCulture;
        Thread.CurrentThread.CurrentUICulture = CultureInfo.InvariantCulture;

        LoggerFactory.DefaultLoggers = new List<ILoggerFrontend>
        {
            new ConsoleLogger(),
            new DiagnosticsLogger(),
            new FileLogger()
        };

        var logger = LoggerFactory.GetLogger("Kernel");

        var asm = Assembly.GetEntryAssembly();
        var version = asm?.GetCustomAttribute<AssemblyInformationalVersionAttribute>()?.InformationalVersion ?? "UNKNOWN";
        var asmVer = asm?.GetName()?.Version?.ToString() ?? "UNKNOWN";

        logger.Info($"\n\n" +
                    $" =======================================================\n" +
                    $" = Starting up Quader\n" +
                    $" = Startup Time: {DateTimeOffset.Now}\n" +
                    $" = Working Directory: {WorkingDirectory}\n" +
                    $" = Assembly Version: {asmVer}\n" +
                    $" = Game Version: {version}\n" +
                    $" =======================================================\n");

        AppDomain.CurrentDomain.UnhandledException += (sender, args) =>
        {
            var ex = args.ExceptionObject as Exception;
            logger.Critical(ex?.ToString() ?? "UNKNOWN EXCEPTION");
        };

        using var game = new GameRoot();
        game.Run();

        logger.Info("Game Closed");
    }
}