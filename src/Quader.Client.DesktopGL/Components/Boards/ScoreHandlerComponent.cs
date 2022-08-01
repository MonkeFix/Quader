using System;
using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.UI;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Skinning;

namespace Quader.Components.Boards
{
    public class ScoreHandlerComponent : RenderableComponent, IUpdatable, IBoardComponent, IResetable
    {
        public override float Width => 1000;
        public override float Height => 10600;
        public Board Board { get; }

        public float Pps { get; private set; }
        public int TotalPieces { get; private set; }
        public int LinesCleared { get; private set; }

        private BoardSkin _boardSkin;

        private string _attackString = "";

        private readonly ILogger _logger = LoggerFactory.GetLogger<ScoreHandlerComponent>();

        public ScoreHandlerComponent(Board board)
        {
            Board = board;

            Board.PieceHardDropped += (sender, boardMove) =>
            {
                TotalPieces++;

                Console.WriteLine($"Attack: {boardMove.Attack}. Combo: {boardMove.Combo}. B2B: {boardMove.BackToBack}. Lines Cleared: {boardMove.LinesCleared}. Modificators: {boardMove.Modificators}");
            };

            Board.LinesCleared += (sender, i) =>
            {
                LinesCleared += i;
                _attackString = string.Join(',', Board.IncomingDamage);
            };

            Board.AttackReceived += (sender, attack) =>
            {
                _attackString = string.Join(',', Board.IncomingDamage);
            };
        }

        public override void OnAddedToEntity()
        {
            _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();
        }

        public void Reset()
        {
            _logger.Debug("Resetting");

            Pps = 0;
            TotalPieces = 0;
            LinesCleared = 0;
        }

        public void Update()
        {
            if (TotalPieces != 0)
            {
                Pps = TotalPieces / Time.TimeSinceSceneLoad;
            }
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            var debugText = $"Pieces on Board: {Board.PiecesOnBoard}\n" +
                            $"Lines Cleared: {LinesCleared}\n" +
                            $"Current Gravity: {Board.CurrentGravity:F3}\n" +
                            $"Current Lock: {Board.CurrentLock:F3}\n" +
                            $"Incoming Garbage: {_attackString}\n" +
                            $"Intermediate Y: {Board.IntermediateY}";

            _boardSkin.DebugFont.DrawInto(
                batcher,
                debugText,
                Entity.Position - new Vector2(180, 90),
                Color.Yellow,
                0f,
                Vector2.Zero,
                Entity.Scale,
                SpriteEffects.None,
                0f
            );


            batcher.DrawString(_boardSkin.MainFont, 
                $"TP: {TotalPieces}\n" +
                $"PPS: {Pps:F1}\n" +
                $"Combo: {Board.CurrentCombo - 1}\n" +
                $"B2B: {Board.CurrentB2B}\n", 
                Entity.Position - new Vector2(180, -200), Color.Red);
        }
    }
}