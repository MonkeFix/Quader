using System.Collections.Generic;
using Microsoft.Xna.Framework;
using Nez;
using Quader.Debugging.Logging;
using Quader.Debugging.Logging.Loggers;
using Quader.Scenes;

namespace Quader
{
    public class GameRoot : Core
    {

        public GameRoot()
        {
            Window.AllowUserResizing = false;

            LoggerFactory.DefaultLoggers = new List<ILogger>
            {
                new ConsoleLogger(),
                new DiagnosticsLogger(),
                new DrawableLogger(Color.Black, 10f, 3f),
                new FileLogger()
            };
        }
        
        protected override void Initialize()
        {
            base.Initialize();

            Scene = new GameplayScene();
        }
    }
}
