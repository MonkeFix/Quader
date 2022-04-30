using System;
using System.IO;
using System.Threading.Tasks;

namespace Quader.Debugging.Logging.Loggers
{
    public class FileLogger : ILogger
    {
        public string FileDir => Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "Logs");

        public string FilePath { get; }

        public FileLogger(string? filePath = null)
        {
            if (filePath == null)
                FilePath = Path.Combine(FileDir,
                    $"log-{DateTime.Now:yyyy-MM-dd}.txt");
            else
                FilePath = filePath;

            if (!Directory.Exists(FileDir))
                Directory.CreateDirectory(FileDir);
        }

        public void Log(object message, LogLevel level)
        {
            using (var sw = new StreamWriter(FilePath, true))
            {
                sw.WriteLine(message);
            }
        }

        public async Task LogAsync(object message, LogLevel level)
        {
            using (var sw = new StreamWriter(FilePath, true))
            {
                await sw.WriteLineAsync(message.ToString());
            }
        }
    }
}
