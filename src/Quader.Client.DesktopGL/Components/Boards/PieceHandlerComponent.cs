using Microsoft.Xna.Framework.Input;
using Nez;
using Quader.Config;
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

        private GameConfig _gameConfig;

        public PieceHandlerComponent(Board board)
        {
            Board = board;

            _gameConfig = Core.Services.GetService<GameConfig>();

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
            if (Input.IsKeyPressed(_gameConfig.Controls.HardDrop))
            {
                var linesCleared = Board.HardDrop();
            }

            var dt = Time.DeltaTime * 1000;

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

            if (Input.IsKeyDown(_gameConfig.Controls.SoftDrop))
            {
                Board.SoftDrop();
            }

            if (Input.IsKeyPressed(_gameConfig.Controls.RotateClockwise))
            {
                Board.Rotate(Rotation.Clockwise);
            }
            if (Input.IsKeyPressed(_gameConfig.Controls.RotateCounterClockwise))
            {
                Board.Rotate(Rotation.CounterClockwise);
            }
            if (Input.IsKeyPressed(_gameConfig.Controls.Rotate180Deg))
            {
                Board.Rotate(Rotation.Deg180);
            }

            if (Input.IsKeyPressed(_gameConfig.Controls.Hold))
            {
                _heldPiece?.HoldPiece();
            }

            if (Input.IsKeyPressed(_gameConfig.Controls.Restart))
            {
                Board.Reset(); // TODO: Restart correctly
            }
        }
    }
}