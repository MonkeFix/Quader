using System;
using System.Linq;
using ColdClearNet;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.UI;
using Quader.Components.Boards.Renderers;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.Pieces;
using Quader.Engine.Replays;
using Quader.Skinning;

namespace Quader.Components.Boards.PieceHandlers
{
    public class PieceHandlerBotComponent : RenderableComponent, IPieceHandler, IDisposable, IBoardComponent, IBoardToggleable
    {
        public override float Width => 1000;
        public override float Height => 1000;

        public Board Board { get; }

        private ColdClear? _coldClear;

        [Inspectable]
        public float TargetPps { get; set; } = 1;//0.1f;
        private float _elapsed;

        [Inspectable] public bool DrawPlan { get; set; } = false;

        private PieceQueueComponent _queue = null!;
        private HeldPieceComponent _hold = null!;

        private BoardSkin _boardSkin;
        private BoardMove _lastMove;

        private bool _holdUsed;
        private PlanPlacement[]? _plan;
        private uint _planSize;

        private int _incomingGarbage;

        private readonly ILogger _logger = LoggerFactory.GetLogger<PieceHandlerBotComponent>();

        public PieceHandlerBotComponent(Board board)
        {
            Board = board;

            _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();

            Board.GarbageReceived += (_, _) =>
            {
                _coldClear?.Reset(Board.ToBoolArray(), _lastMove.Combo, _lastMove.BackToBack > 0);
            };

            Board.AttackReceived += (_, attack) =>
            {
                _incomingGarbage = attack;
                //_coldClear?.RequestNextMove(attack);
            };
        }

        public void Start()
        {
            _queue = Entity.GetComponent<PieceQueueComponent>();
            _hold = Entity.GetComponent<HeldPieceComponent>();

            _elapsed = 0;
            _holdUsed = false;

            InitColdClear();
        }

        private void InitColdClear()
        {
            if (_coldClear != null)
            {
                _coldClear.Dispose();
                _coldClear = null;
            }

            var q = _queue.Queue.Select(p => PieceTypeToPiece(p.Type)).ToList();
            q.Add(PieceTypeToPiece(_queue.NextPiece.Type));

            var opt = ColdClear.DefaultOptions;
            opt.UseHold = true;
            opt.Speculate = true;
            opt.SpawnRule = SpawnRule.Row21AndFall;
            opt.MaxNodes = (uint)Math.Pow(2, 20);

            _coldClear = new ColdClear(
                opt,
                ColdClear.DefaultWeights,
                q
            );
        }

        public void Update()
        {
            if (_coldClear == null)
                return;

            var dt = Time.DeltaTime;
            _elapsed += dt;

            if (TargetPps <= 0)
            {
                DoMove();
            }
            else if (_elapsed >= 1f / TargetPps)
            {
                DoMove();

                _elapsed = 0;
            }
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            batcher.DrawString(Graphics.Instance.BitmapFont, $"Incoming Garbage: {_incomingGarbage}", new Vector2(0, 0),
                Color.White);

            if (DrawPlan && _plan != null)
            {
                for (int i = 0; i < _planSize; i++)
                {
                    var plan = _plan[i];

                    for (int j = 0; j < 4; j++)
                    {
                        var rawX = plan.ExpectedX[j];
                        var rawY = plan.ExpectedY[j];
                        var p = PieceToPieceType(plan.Piece);

                        var drawX = Entity.Position.X + rawX * 32;
                        var drawY = Entity.Position.Y + (19 - rawY) * 32;

                        batcher.Draw(
                            _boardSkin.GhostSprite,
                            new Vector2(drawX, drawY),
                            PieceUtils.GetColorByPieceType(p) * 0.5f,
                            0,
                            Vector2.Zero,
                            1f,
                            SpriteEffects.None,
                            0
                        );
                    }
                }

            }
        }

        public void Dispose()
        {
            _coldClear?.Dispose();
            _coldClear = null;
        }

        private void DoMove()
        {
            if (_coldClear == null)
                return;

            var incomingGarbage = Board.IncomingDamage.LastOrDefault(0);
            _coldClear.RequestNextMove(incomingGarbage);

            var pl = 5;

            // TODO: Make bot async via PollNextMove method
            var move = _coldClear.BlockNextMove(pl);

            _plan = move.PlanPlacement.ToArray();
            _planSize = (uint)pl;


            if (move.PollStatus == BotPollStatus.MoveProvided)
            {
                if (move.Move.Hold)
                {
                    _hold.HoldPiece();
                    if (!_holdUsed)
                    {
                        _coldClear.AddNextPieceAsync(PieceTypeToPiece(_queue.NextPiece.Type));
                        _holdUsed = true;
                    }
                }

                for (int i = 0; i < move.Move.MovementCount; i++)
                {
                    DoMove(move.Move.Movements[i]);
                }

                _lastMove = Board.HardDrop();

                _incomingGarbage = 0;  //Math.Max(0, _incomingGarbage - Board.CalculateAttack(_lastMove));
            }
            else if (move.PollStatus == BotPollStatus.Waiting)
            {
                Console.WriteLine("Bot is waiting");
            }
            else
            {
                Console.WriteLine("Bot is dead");
            }

            if (_queue != null && _coldClear != null)
            {
                var np = _queue.NextPiece;
                _coldClear.AddNextPieceAsync(PieceTypeToPiece(np.Type));
            }
        }

        private void DoMove(Movement m)
        {
            switch (m)
            {
                case Movement.Left:
                    Board.PieceMoveLeft();
                    break;
                case Movement.Right:
                    Board.PieceMoveRight();
                    break;
                case Movement.Clockwise:
                    Board.Rotate(Rotation.Clockwise);
                    break;
                case Movement.CounterClockwise:
                    Board.Rotate(Rotation.CounterClockwise);
                    break;
                case Movement.Drop:
                    Board.SoftDrop(100);
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        private Piece PieceTypeToPiece(PieceType type)
        {
            return type switch
            {
                PieceType.I => Piece.I,
                PieceType.O => Piece.O,
                PieceType.T => Piece.T,
                PieceType.L => Piece.L,
                PieceType.J => Piece.J,
                PieceType.S => Piece.S,
                PieceType.Z => Piece.Z,
                _ => throw new ArgumentOutOfRangeException(nameof(type), type, null)
            };
        }

        private PieceType PieceToPieceType(Piece p)
        {
            return p switch
            {
                Piece.I => PieceType.I,
                Piece.O => PieceType.O,
                Piece.T => PieceType.T,
                Piece.L => PieceType.L,
                Piece.J => PieceType.J,
                Piece.S => PieceType.S,
                Piece.Z => PieceType.Z,
                _ => throw new ArgumentOutOfRangeException(nameof(p), p, null)
            };
        }

        public void Enable()
        {
            Enabled = true;
        }

        public void Disable()
        {
            Enabled = false;
            Dispose();
        }
    }
}