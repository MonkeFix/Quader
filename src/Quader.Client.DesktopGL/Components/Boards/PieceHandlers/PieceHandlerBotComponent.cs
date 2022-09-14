using System;
using System.Linq;
using ColdClearNet;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.UI;
using Quader.Bot.Api;
using Quader.Components.Boards.Renderers;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.Pieces;
using Quader.Engine.Replays;
using Quader.Managers;
using Quader.Skinning;

namespace Quader.Components.Boards.PieceHandlers
{
    public class PieceHandlerBotComponent : Component, IPieceHandler, IDisposable, IBoardComponent, IBoardToggleable
    {
        public Board Board { get; }

        // private ColdClear? _coldClear;

        [Inspectable]
        public float TargetPps { get; set; } = 1;//0.1f;
        private float _elapsed;

        [Inspectable] public bool DrawPlan { get; set; } = false;

        private PieceQueueComponent _queue = null!;
        private HeldPieceComponent _hold = null!;

        private BoardSkin _boardSkin;
        private BoardMove _lastMove;

        private bool _holdUsed;

        private readonly ILogger _logger = LoggerFactory.GetLogger<PieceHandlerBotComponent>();

        private BotIpcManager? _botManager;

        public PieceHandlerBotComponent(Board board)
        {
            Board = board;

            _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();

            Board.GarbageReceived += (_, _) =>
            {
                _botManager?.Reset(Board.ToBoolArray(), _lastMove.Combo, _lastMove.BackToBack > 0);
            };

            Board.AttackReceived += (_, attack) =>
            {
                //_coldClear?.RequestNextMove(attack);
            };
        }

        ~PieceHandlerBotComponent()
        {
            Dispose();
        }

        public void Start()
        {
            _queue = Entity.GetComponent<PieceQueueComponent>();
            _hold = Entity.GetComponent<HeldPieceComponent>();

            _elapsed = 0;
            _holdUsed = false;

            InitColdClear();
        }

        public override void OnAddedToEntity()
        {
            base.OnAddedToEntity();
        }

        public override void OnRemovedFromEntity()
        {
            Dispose();
        }

        private void InitColdClear()
        {
            if (_botManager != null)
            {
                Dispose();
            }

            var q = _queue.Queue.Select(p => PieceTypeToPiece(p.Type)).ToList();
            q.Add(PieceTypeToPiece(_queue.NextPiece.Type));

            _botManager = new BotIpcManager();
            _botManager.Start(q.ToArray());

            /*if (_coldClear != null)
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
            );*/
        }

        public void Update()
        {
            if (_botManager == null || !Enabled)
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

        public void Dispose()
        {
            _botManager?.Stop();
            _botManager?.Dispose();
            _botManager = null;
        }

        private void DoMove()
        {
            if (_botManager == null)
                return;

            var incomingGarbage = Board.IncomingDamage.Sum();

            var move = _botManager.DoMove(incomingGarbage);

            if (move.Status == BotStatus.MoveProvided)
            {
                if (move.IsHoldUsed)
                {
                    _hold.HoldPiece();
                    if (!_holdUsed)
                    {
                        _botManager.PushPiece(PieceTypeToPiece(_queue.NextPiece.Type));
                        _holdUsed = true;
                    }
                }

                for (int i = 0; i < move.MovementCount; i++)
                {
                    DoMove(move.Movements[i]);
                }

                _lastMove = Board.HardDrop();
            }
            else if (move.Status == BotStatus.Waiting)
            {
                _logger.Info("Bot Is Waiting");
            }
            else
            {
                _logger.Info("Bot Is Dead");
            }

            if (_queue != null && _botManager != null)
            {
                var np = _queue.NextPiece;
                _botManager.PushPiece(PieceTypeToPiece(np.Type));
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
            Dispose();
        }

        public void Disable()
        {
            Enabled = false;
            Dispose();
        }
    }
}