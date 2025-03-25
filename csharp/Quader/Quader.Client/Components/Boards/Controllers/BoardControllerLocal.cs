using Microsoft.Xna.Framework.Input;
using Nez;
using Quader.Client.GameConfig;
using Quader.Engine;
using Quader.Engine.Boards;
using Quader.Engine.Pieces;
using Quader.Engine.Replays;

namespace Quader.Client.Components.Boards.Controllers;

public class BoardControllerLocal : Component, IBoardController
{
    class PieceMover
    {
        public float Elapsed;
        public float Arr;
        public float Das;
        public int Sdf;
        public bool IsLeftDown;
        public bool IsRightDown;

        public void MoveLeft(Board board) => board.MoveLeft(1);
        public void MoveRight(Board board) => board.MoveRight(1);
        public void SoftDrop(Board board) => board.SoftDrop(Sdf);

        public void Reset()
        {
            Elapsed = 0;
            IsLeftDown = false;
            IsRightDown = false;
        }
    }

    public Board Board { get; }
    private Config _config;
    private PieceMover _mover;

    public BoardControllerLocal(Board board)
    {
        Board = board;
    }

    public override void OnAddedToEntity()
    {
        _config = Core.Services.GetService<Config>();
        _mover = new PieceMover
        {
            Arr = _config.Handling.AutomaticRepeatRate,
            Das = _config.Handling.DelayedAutoShift,
            Sdf = _config.Handling.SoftDropFactor
        };
    }

    public void Update()
    {
        if (Input.IsKeyPressed(_config.Controls.HardDrop))
            Board.HardDrop(out var result);

        if (Input.IsKeyPressed(_config.Controls.RotateClockwise))
            Board.TryRotate(RotationDirection.Clockwise, out _);
        if (Input.IsKeyPressed(_config.Controls.RotateCounterClockwise))
            Board.TryRotate(RotationDirection.CounterClockwise, out _);
        if (Input.IsKeyPressed(_config.Controls.Rotate180Deg))
            Board.TryRotate(RotationDirection.Deg180, out _);

        if (Input.IsKeyPressed(_config.Controls.Hold))
            Board.TryHoldPiece(out _);

        if (Input.IsKeyDown(_config.Controls.SoftDrop))
            _mover.SoftDrop(Board);

        // ==== LEFT ====
        if (Input.IsKeyPressed(Keys.Left))
            _mover.MoveLeft(Board);
        if (Input.IsKeyDown(Keys.Left))
        {
            _mover.IsLeftDown = true;
            _mover.Elapsed += Time.DeltaTime * 1000;
        }

        if (Input.IsKeyReleased(Keys.Left))
        {
            _mover.IsLeftDown = false;
            _mover.Elapsed = 0;
        }

        // ==== RIGHT ====
        if (Input.IsKeyPressed(Keys.Right))
            _mover.MoveRight(Board);
        if (Input.IsKeyDown(Keys.Right))
        {
            _mover.IsRightDown = true;
            _mover.Elapsed += Time.DeltaTime * 1000;
        }

        if (Input.IsKeyReleased(Keys.Right))
        {
            _mover.IsRightDown = false;
            _mover.Elapsed = 0;
        }

        if (_mover.Elapsed >= _mover.Das)
        {
            var moves = 10;

            for (int i = 0; i < moves; i++)
            {
                if (_mover.IsLeftDown) _mover.MoveLeft(Board);
                if (_mover.IsRightDown) _mover.MoveRight(Board);
            }
        }

        if (Input.IsKeyPressed(_config.Controls.Restart))
            Board.Reset(Environment.TickCount);
    }
}