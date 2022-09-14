using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using ColdClearNet;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Input;
using Nez;
using Nez.ImGuiTools;
using Nez.Persistence;
using Nez.Sprites;
using Nez.Timers;
using Nez.UI;
using Quader.Components;
using Quader.Components.UI;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.PieceGenerators;
using Quader.Engine.Pieces;
using Quader.Engine.Replays;
using Quader.Engine.Settings;
using Quader.Managers;
using Quader.Serialization;
using Quader.Skinning;

namespace Quader.Scenes
{
    public class GameplayScene : Scene
    {
        public const int ScreenSpaceRenderLayer = 999;
        public const int BoardTag = 1000;

        public readonly int Width = 1600;
        public readonly int Height = 980;

        private readonly ILogger _logger = LoggerFactory.GetLogger<GameplayScene>();

        private BoardManagerComponent _boardManager;

        public GameplayScene()
        {
            _logger.Debug("Constructing, adding renderer");
            AddRenderer(new DefaultRenderer(0));
            AddRenderer(new ScreenSpaceRenderer(100, ScreenSpaceRenderLayer));
            AddRenderer(new RenderLayerExcludeRenderer(0, ScreenSpaceRenderLayer));
        }

        public override void Initialize()
        {
            base.Initialize();
            ClearColor = Color.Black; 

            _logger.Debug("Initializing");

            SetDesignResolution(Width, Height, SceneResolutionPolicy.BestFit);
            Screen.SetSize(1920, 1080);

            /*var ui = new Entity("ui");
            var canvas = ui.AddComponent(new UICanvas());

            var table = canvas.Stage.AddElement(new Table());
            var b = table.Add(new TextButton("Hello!", skin));
            b.Width(256);
            b.Height(64);

            AddEntity(ui);*/

            var sharedActions = new SharedActions();

            CreateEntity("shared-ui").AddComponent(new SharedUiComponent(sharedActions));

            _logger.Debug("Creating Board Entity...");

            _boardManager = CreateEntity("board-manager")
                .AddComponent(new BoardManagerComponent(sharedActions));

            /*Core.Schedule(2f, true, boardBot, (timer) =>
            {
                timer.GetContext<Board>().PushGarbage(1);
            });

            _logger.Debug("Done initializing");*/

            for (int i = 0; i < 1; i++)
            {
                var botPipeManager = new BotIpcManager();
                botPipeManager.Start(new[] { Piece.I, Piece.J, Piece.L, Piece.O, Piece.S, Piece.T, Piece.Z, Piece.I, Piece.J, Piece.L, Piece.O, Piece.S, Piece.T, Piece.Z, });
                botPipeManager.Reset(new bool[400], 0, false);
                var m = botPipeManager.DoMove(0);
                botPipeManager.DoMove(0);
                botPipeManager.PushPiece(Piece.O);
                botPipeManager.DoMove(0);
                botPipeManager.PushPiece(Piece.T);
                botPipeManager.Stop();

                // Thread.Sleep(2000);
            }
        }
    }
}