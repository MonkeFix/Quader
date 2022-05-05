using Microsoft.Xna.Framework.Input;
using Nez;
using Quader.Engine;
using Quader.Engine.Pieces;

namespace Quader.Components.Boards
{
    public class PieceHandlerComponent : Component, IUpdatable
    {
        public Board Board { get; }

        private HeldPieceComponent? _heldPiece;

        public PieceHandlerComponent(Board board)
        {
            Board = board;
        }

        public override void OnAddedToEntity()
        {
            _heldPiece = Entity.GetComponent<HeldPieceComponent>();
        }

        public void Update()
        {
            if (Input.IsKeyPressed(Keys.Space))
            {
                var linesCleared = Board.HardDrop();
            }

            if (Input.IsKeyPressed(Keys.Left))
            {
                Board.MoveLeft();
            }

            if (Input.IsKeyPressed(Keys.Right))
            {
                Board.MoveRight();
            }

            if (Input.IsKeyDown(Keys.Down))
            {
                Board.SoftDrop();
            }

            if (Input.IsKeyPressed(Keys.X))
            {
                Board.Rotate(Rotation.Clockwise);
            }
            if (Input.IsKeyPressed(Keys.Z))
            {
                Board.Rotate(Rotation.CounterClockwise);
            }
            if (Input.IsKeyPressed(Keys.F))
            {
                Board.Rotate(Rotation.Deg180);
            }

            if (Input.IsKeyPressed(Keys.C))
            {
                _heldPiece?.HoldPiece();
            }
        }
    }
}