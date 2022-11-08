using System;
using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.UI;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.Replays;
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

        private TimeManagerComponent _timeManager = null!;

        private int _totalQuads;
        private int _totalTriples;
        private int _totalDoubles;
        private int _totalTSpins;
        private int _totalTSpinMinis;
        private int _totalAllClears;
        private int _totalTSpinSingles;
        private int _totalTSpinDoubles;
        private int _totalTSpinTriples;
        private int _currentCombo;
        private int _maxCombo;
        private int _currentB2b;
        private int _maxB2b;

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

                if (boardMove.Modificators.HasFlag(BoardHardDropInfoModificators.TSpin))
                {
                    _totalTSpins++;
                    if (boardMove.Modificators.HasFlag(BoardHardDropInfoModificators.Single))
                        _totalTSpinSingles++;
                    else if (boardMove.Modificators.HasFlag(BoardHardDropInfoModificators.Double))
                        _totalTSpinDoubles++;
                    else if (boardMove.Modificators.HasFlag(BoardHardDropInfoModificators.Triple))
                        _totalTSpinTriples++;
                }
                if (boardMove.Modificators.HasFlag(BoardHardDropInfoModificators.TSpinMini))
                {
                    _totalTSpinMinis++;
                    if (boardMove.Modificators.HasFlag(BoardHardDropInfoModificators.Single))
                        _totalTSpinSingles++;
                    else if (boardMove.Modificators.HasFlag(BoardHardDropInfoModificators.Double))
                        _totalTSpinDoubles++;
                    else if (boardMove.Modificators.HasFlag(BoardHardDropInfoModificators.Triple))
                        _totalTSpinTriples++;
                }
                if (boardMove.Modificators.HasFlag(BoardHardDropInfoModificators.Double))
                    _totalDoubles++;
                if (boardMove.Modificators.HasFlag(BoardHardDropInfoModificators.Triple))
                    _totalTriples++;
                if (boardMove.Modificators.HasFlag(BoardHardDropInfoModificators.Quad))
                    _totalQuads++;
                if (boardMove.Modificators.HasFlag(BoardHardDropInfoModificators.AllClear))
                    _totalAllClears++;

                _currentCombo = boardMove.Combo;
                _currentB2b = boardMove.BackToBack;

                if (_currentCombo > _maxCombo)
                    _maxCombo = _currentCombo;
                if (_currentB2b > _maxB2b)
                    _maxB2b = _currentB2b;

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

        public override void OnAddedToEntity()
        {
            _timeManager = Entity.GetComponent<TimeManagerComponent>();
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
            _logger.Trace("Enabling");
            _elapsedSeconds = 0;
            _isEnabled = true;
        }

        public void Disable()
        {
            _logger.Trace("Disabling");
            _isEnabled = false;
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            var pressedKeys = Input.CurrentKeyboardState.GetPressedKeys().Select(k => k.ToString());
            var debugText = $"Pieces on Board: {Board.PiecesOnBoard} | PK: {string.Join(", ", pressedKeys)}\n" +
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
                $"APM: {Apm:F2}\n" +
                $"";

            if (Board.CurrentCombo > 1)
                scoreStr += $"\nCombo: {Board.CurrentCombo - 1}";

            if (Board.CurrentB2B > 0)
                scoreStr += $"\nB2B: {Board.CurrentB2B}";

            // scoreStr += "\n\n\n\n\n\n\n\n\n\n" +
            //     $"DT: {_timeManager.DeltaTime:F4}\n" +
            //     $"Start: {_timeManager.StartTimeUtc}\n" + 
            //     $"UC: {_timeManager.UpdateCycles}\n" +
            //     $"MS: {_timeManager.ElapsedMilliseconds:F0}\n" +
            //     $"Sec: {_timeManager.ElapsedSeconds:F3}\n" +
            //     $"Min: {_timeManager.ElapsedMinutes:F2}";


            batcher.DrawString(
                _boardSkin.MainFont,
                scoreStr,
                Entity.Position - new Vector2(180, -200), Color.Red
            );

            var statsStr =
                $"Doubles: {_totalDoubles}\n" +
                $"Triples: {_totalTriples}\n" +
                $"Quads: {_totalQuads}\n" +
                $"T-Spins: {_totalTSpins}\n" +
                $"T-Spin Minis: {_totalTSpinMinis}\n" +
                $"T-Spin Single: {_totalTSpinSingles}\n" +
                $"T-Spin Double: {_totalTSpinDoubles}\n" +
                $"T-Spin Triples: {_totalTSpinTriples}\n" +
                $"All Clears: {_totalAllClears}\n" +
                $"Max Combo: {_maxCombo}\n" +
                $"Max B2B: {_maxB2b}";

            batcher.DrawString(
                _boardSkin.MainFont,
                statsStr,
                Entity.Position + new Vector2(440, 0),
                Color.White
            );
        }
    }
}
