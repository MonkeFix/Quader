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

        private VirtualIntegerAxis _moveInput;
        private VirtualButton _rotateClockwise;
        private VirtualButton _rotateCounterClockwise;

        private VirtualButton _moveLeft;
        private VirtualButton _moveRight;

        public PieceHandlerComponent(Board board)
        {
            Board = board;

            _moveInput = new VirtualIntegerAxis()
                .AddGamePadDPadLeftRight()
                .AddGamePadLeftStickX()
                .AddKeyboardKeys(VirtualInput.OverlapBehavior.TakeNewer, Keys.Left, Keys.Right);


            _moveLeft = new VirtualButton().AddKeyboardKey(Keys.Left);
            _moveLeft.SetRepeat(0.15f, 0.0000001f);
            _moveRight = new VirtualButton().AddKeyboardKey(Keys.Right);
            _moveRight.SetRepeat(0.1f, 0.0000001f);
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

            /*if (Input.IsKeyPressed(Keys.Left))
            {
                Board.MoveLeft();
            }

            if (Input.IsKeyPressed(Keys.Right))
            {
                Board.MoveRight();
            }*/

            if (_moveLeft.IsPressed || _moveLeft.IsRepeating)
                Board.MoveLeft();

            if (_moveRight.IsPressed || _moveRight.IsRepeating)
                Board.MoveRight();

            /*var dir = _moveInput.Value;
            if (dir != 0)
                Board.Move(dir);*/

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