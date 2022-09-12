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
    public class ScoreHandlerComponent : RenderableComponent, IUpdatable, IBoardComponent, IBoardToggleable
    {
        public override float Width { get; }
        public override float Height { get; }

        // public override RectangleF Bounds { get; }
        public Board Board { get; }

        /// <summary>
        /// Gets Pieces Per Second
        /// </summary>
        public float Pps { get; private set; }
        /// <summary>
        /// Gets Attack Per Minute
        /// </summary>
        public float Apm { get; private set; }
        public int TotalPieces { get; private set; }
        public int LinesCleared { get; private set; }

        private BoardSkin _boardSkin;

        private string _attackString = "";
        private int _incomingDamageTotal = 0;

        private readonly ILogger _logger = LoggerFactory.GetLogger<ScoreHandlerComponent>();

        private bool _isEnabled = true;

        private int _attackTotal;

        private float _elapsedSeconds = 0;
        private float _elapsedMinutes = 0;

        public ScoreHandlerComponent(Board board)
        {
            _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();

            Board = board;

            Width = _boardSkin.Table.ScoresRect.Width;
            Height = _boardSkin.Table.ScoresRect.Height;

            Board.PieceHardDropped += (sender, boardMove) =>
            {
                TotalPieces++;

                _attackTotal += boardMove.Attack;

                _logger.Trace($"Attack: {boardMove.Attack}. Combo: {boardMove.Combo}. B2B: {boardMove.BackToBack}. Lines Cleared: {boardMove.LinesCleared}. Modificators: {boardMove.Modificators}");
            };

            Board.LinesCleared += (sender, i) =>
            {
                LinesCleared += i;
                _attackString = string.Join(',', Board.IncomingDamage);
                _incomingDamageTotal = Board.IncomingDamage.Sum(i1 => i1);
            };

            Board.AttackReceived += (sender, attack) =>
            {
                _attackString = string.Join(',', Board.IncomingDamage);
                _incomingDamageTotal = Board.IncomingDamage.Sum(i1 => i1);
            };

            /*Core.Schedule(1, true, (timer) =>
            {
                _logger.Info($"{_elapsed.TotalSeconds}/{_elapsedSeconds}");
            });*/
        }

        public void Update()
        {
            // _msSinceStartup += Time.DeltaTime * GameRoot.TargetFps;
            if (_isEnabled)
            {
                _elapsedSeconds += Time.DeltaTime;
                _elapsedMinutes += Time.DeltaTime / 60;

                Pps = TotalPieces / _elapsedSeconds;
                Apm = _attackTotal / _elapsedMinutes;
            }
        }

        public void Enable()
        {
            _elapsedSeconds = 0;
            _isEnabled = true;
        }

        public void Disable()
        {
            _isEnabled = false;
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            var debugText = $"Pieces on Board: {Board.PiecesOnBoard}\n" +
                            $"Lines Cleared: {LinesCleared}\n" +
                            $"Current Gravity: {Board.CurrentGravity:F3}\n" +
                            $"Current Lock: {Board.CurrentLock:F3}\n" +
                            $"Incoming Garbage ({_incomingDamageTotal}): {_attackString}\n" +
                            $"Intermediate Y: {Board.IntermediateY}\n" +
                            $"Garbage Delay Cooldown: {Board.GarbageDelayCooldown} ({Board.GarbageDelayMs})";

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

            var scoreStr =
                $"{_elapsedSeconds:F3}" + "\n" +
                $"TP: {TotalPieces}\n" +
                $"PPS: {Pps:F2}\n" +
                $"APM: {Apm:F2}";

            if (Board.CurrentCombo > 1)
                scoreStr += $"\nCombo: {Board.CurrentCombo - 1}";

            if (Board.CurrentB2B > 0)
                scoreStr += $"\nB2B: {Board.CurrentB2B}";


            batcher.DrawString(
                _boardSkin.MainFont,
                scoreStr,
                Entity.Position - new Vector2(180, -200), Color.Red
            );
        }
    }
}