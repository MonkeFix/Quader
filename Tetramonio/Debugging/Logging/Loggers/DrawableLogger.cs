using System.Threading.Tasks;
using Microsoft.Xna.Framework;
using Nez;

namespace Tetramonio.Debugging.Logging.Loggers
{
    public class DrawableLogger : LoggerBase
    {
        private Color _color;
        private float _duration;
        private float _scale;

        public DrawableLogger(Color color, float duration = 3f, float scale = 1f)
        {
            _color = color;
            _duration = duration;
            _scale = scale;
        }

        public override void Log(string message, LogLevel level)
        {
            Debug.DrawText(message, _color, _duration, _scale);
        }

        public override Task LogAsync(string message, LogLevel level)
        {
            Debug.DrawText(message, _color, _duration, _scale);
            return Task.CompletedTask;
        }
        
    }
}