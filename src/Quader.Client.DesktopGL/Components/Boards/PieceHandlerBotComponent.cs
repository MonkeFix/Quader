using System;
using System.Linq;
using System.Reflection.Metadata.Ecma335;
using System.Threading;
using System.Threading.Tasks;
using ColdClearNet;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.UI;
using Quader.Engine;
using Quader.Engine.Pieces;
using Quader.Skinning;

namespace Quader.Components.Boards
{
    public class PieceHandlerBotComponent : RenderableComponent, IUpdatable, IDisposable, IBoardComponent
    {
        public override float Width => 1000;
        public override float Height => 1000;

        public Board Board { get; }

        private ColdClear _coldClear = null!;

        [Inspectable]
        public float TargetPps { get; set; } = 1;//0.1f;
        private float _elapsed;

        [Inspectable] public bool DrawPlan { get; set; } = false;

        private PieceQueueComponent _queue = null!;
        private HeldPieceComponent _hold = null!;

        private BoardSkin _boardSkin;

        public PieceHandlerBotComponent(Board board)
        {
            Board = board;

            _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();
        }

        public override void OnAddedToEntity()
        {
            _queue = Entity.GetComponent<PieceQueueComponent>();
            _hold = Entity.GetComponent<HeldPieceComponent>();

            var q = _queue.Queue.Select(p => PieceTypeToPiece(p.Type)).ToList();
            q.Add(PieceTypeToPiece(_queue.NextPiece.Type));

            var opt = ColdClear.DefaultOptions;
            opt.UseHold = true;
            opt.Speculate = true;
            opt.SpawnRule = SpawnRule.Row21AndFall;

            _coldClear = new ColdClear(
                opt,
                ColdClear.DefaultWeights,
                q
            );
        }

        public void Update()
        {
            var dt = Time.DeltaTime;
            _elapsed += dt;

            if (TargetPps <= 0)
            {
                DoMove();
            }
            else if (_elapsed >= 1f/TargetPps)
            {
                DoMove();

                _elapsed = 0;
            }
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            if (DrawPlan)
            {
                for (int i = 0; i < _planSize; i++)
                {
                    var plan = _plan[i];

                    for (int j = 0; j < 4; j++)
                    {
                        var rawX = plan.ExpectedX[j];
                        var rawY = plan.ExpectedY[j];
                        var p = PieceToPieceType(plan.Piece);

                        var drawX = Entity.Position.X + (rawX * 32);
                        var drawY = Entity.Position.Y + ((19 - rawY) * 32);

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
        }

        private bool _holdUsed = false;
        private PlanPlacement[] _plan;
        private uint _planSize;

        private void DoMove()
        {
            _coldClear.RequestNextMove(0);
            var move = new Move();
            var pp = new PlanPlacement[5];
            var pl = 5U;

            // TODO: Make bot async via PollNextMove method
            var pollStatus = _coldClear.BlockNextMove(move, pp, ref pl);

            _plan = pp;
            _planSize = pl;

            if (pl > 0)
            {
                //Console.WriteLine();
            }


            if (pollStatus == BotPollStatus.MoveProvided)
            {
                if (move.Hold)
                {
                    _hold.HoldPiece();
                    if (!_holdUsed)
                    {
                        _coldClear.AddNextPieceAsync(PieceTypeToPiece(_queue.NextPiece.Type));
                        _holdUsed = true;
                    }
                }

                /*for (int i = 0; i < 4; i++)
                {
                    var rawX = move.ExpectedX[i];
                    var rawY = move.ExpectedY[i];

                    Board.SetCellAt(rawX, 39 - rawY, Board.CurrentPiece.BoardCellType);
                    
                }
                Board.PushPiece(_queue.Request());
                Board.ForceUpdate();*/
                //Console.Write("Moves: ");

                var moves = move.Movements;
                for (int i = 0; i < move.MovementCount; i++)
                {
                    var m = moves[i];
                    //Console.Write($"{m}\t");
                    switch (m)
                    {
                        case Movement.Left:
                            Board.MoveLeft();
                            break;
                        case Movement.Right:
                            Board.MoveRight();
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

                //Console.WriteLine();

                var lines = Board.HardDrop();
                if (lines.Type.HasFlag(BoardMoveType.AllClear))
                    Console.WriteLine("ALL CLEAR!");

                if (lines.LinesCleared > 0)
                    Console.WriteLine($"Lines Cleared: {lines.LinesCleared}. B2B: {lines.BackToBack}. Combo: {lines.Combo}. Type: {lines.Type}");
                /*if (lines > 0)
                    Console.WriteLine("Cleared Lines by the Bot: " + lines);*/
            }
            else if (pollStatus == BotPollStatus.Waiting)
            {
                Console.WriteLine("Bot is waiting");
            }
            else
            {
                Console.WriteLine("Bot is dead");
            }

            var np = _queue.NextPiece;
            _coldClear.AddNextPieceAsync(PieceTypeToPiece(np.Type));
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
    }
}