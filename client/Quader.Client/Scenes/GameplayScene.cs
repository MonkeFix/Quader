using System;
using LiteLog.Logging;
using Microsoft.Xna.Framework;
using Nez;
using Quader.Components;
using Quader.Components.UI;

namespace Quader.Scenes
{
    public class GameplayScene : SceneBase
    {
        private readonly ILogger _logger = LoggerFactory.GetLogger<GameplayScene>();

        private BoardManagerComponent _boardManager = null!;
        private SharedActions _sharedActions = null!;

        public GameplayScene() : base(true, true)
        {
            
        }

        public override void Initialize()
        {
            base.Initialize();

            _logger.Debug("Initializing");

            _sharedActions = new SharedActions();
            _sharedActions.OpenReplaysAction = () =>
            {
                var t = Core.StartSceneTransition(new FadeTransition(() => new ReplayScene(_sharedActions)));
            };

            CreateEntity("shared-ui").AddComponent(new SharedUiComponent(_sharedActions));

            _logger.Debug("Creating Board Entity...");

            _boardManager = CreateEntity("board-manager")
                .AddComponent(new BoardManagerComponent(_sharedActions));

            /*Core.Schedule(2f, true, boardBot, (timer) =>
            {
                timer.GetContext<Board>().PushGarbage(1);
            });

            _logger.Debug("Done initializing");*/
        }
    }
}