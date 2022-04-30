using System.Threading.Tasks;
using Microsoft.Xna.Framework;
using Nez;

namespace Quader.Debugging.Logging.Loggers
{
    public class DrawableLogger : ILogger
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

        public void Log(object message, LogLevel level)
        {
            Debug.DrawText(message.ToString(), _color, _duration, _scale);
        }

        public Task LogAsync(object message, LogLevel level)
        {
            Debug.DrawText(message.ToString(), _color, _duration, _scale);
            return Task.CompletedTask;
        }
        
    }
}