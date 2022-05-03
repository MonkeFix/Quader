using System;
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

        public static GameTime GameTime { get; private set; }
        
        public GameRoot()
            : base(windowTitle: "Quader")
        {
            Window.AllowUserResizing = false;

            LoggerFactory.DefaultLoggers = new List<ILoggerFrontend>
            {
                new ConsoleLogger(),
                new DiagnosticsLogger(),
                new DrawableLogger(Color.Black, 10f, 3f),
                new FileLogger()
            };

            IsFixedTimeStep = false;
            TargetElapsedTime = TimeSpan.FromSeconds(1.0 / 240.0);
        }
        
        protected override void Initialize()
        {
            base.Initialize();

            Scene = new GameplayScene();
        }

        protected override void Update(GameTime gameTime)
        {
            base.Update(gameTime);

            GameTime = gameTime;
        }
    }
}
