using System;
using Nez;
using Quader.Config;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.Pieces;

namespace Quader.Components.Boards
{
    public class PieceHandlerPlayerComponent : Component, IPieceHandler, IBoardComponent, IResetable
    {
        public Board Board { get; }

        private HeldPieceComponent? _heldPiece;

        /*private VirtualIntegerAxis _moveInput;
        private VirtualButton _rotateClockwise;
        private VirtualButton _rotateCounterClockwise;

        private VirtualButton _moveLeft;
        private VirtualButton _moveRight;*/

        private readonly GameConfig _gameConfig;
        private readonly Controls _controls;

        private float _elapsed;
        private readonly float _arr;
        private readonly float _das;
        private readonly int _sdf;

        private bool _isLeftDown;
        private bool _isRightDown;

        private readonly Action _restartAction;

        private readonly ILogger _logger = LoggerFactory.GetLogger<PieceHandlerPlayerComponent>();

        public PieceHandlerPlayerComponent(Board board, Action restartAction)
        {
            Board = board;
            _restartAction = restartAction;

            _gameConfig = Core.Services.GetService<GameConfig>();
            _arr = _gameConfig.Handling.AutomaticRepeatRate;
            _das = _gameConfig.Handling.DelayedAutoShift;
            _sdf = _gameConfig.Handling.SoftDropFactor;

            _controls = _gameConfig.Controls;

            /*_moveInput = new VirtualIntegerAxis()
                .AddGamePadDPadLeftRight()
                .AddGamePadLeftStickX()
                .AddKeyboardKeys(VirtualInput.OverlapBehavior.TakeNewer, Keys.Left, Keys.Right);


            _moveLeft = new VirtualButton().AddKeyboardKey(Keys.Left);
            _moveLeft.SetRepeat(0.15f, 0.0000001f);
            _moveRight = new VirtualButton().AddKeyboardKey(Keys.Right);
            _moveRight.SetRepeat(0.1f, 0.0000001f);*/
        }

        public override void OnAddedToEntity()
        {
            _heldPiece = Entity.GetComponent<HeldPieceComponent>();
        }

        public void Reset()
        {
            _logger.Debug("Resetting");

            _elapsed = 0;
            _isLeftDown = false;
            _isRightDown = false;
        }

        public void Update()
        {
            if (Input.IsKeyPressed(_gameConfig.Controls.HardDrop))
            {
                Board.HardDrop();
            }

            var dt = Time.DeltaTime * 1000;


            if (Input.IsKeyPressed(_controls.MoveLeft))
                Board.PieceMoveLeft();
            if (Input.IsKeyDown(_controls.MoveLeft))
            {
                _isLeftDown = true;
                _elapsed += dt;
            }
            if (Input.IsKeyReleased(_controls.MoveLeft))
            {
                _elapsed = 0;
                _isLeftDown = false;
            }

            if (Input.IsKeyPressed(_controls.MoveRight))
                Board.PieceMoveRight();
            if (Input.IsKeyDown(_controls.MoveRight))
            {
                _isRightDown = true;
                _elapsed += dt;
            }
            if (Input.IsKeyReleased(_controls.MoveRight))
            {
                _isRightDown = false;
                _elapsed = 0;
            }

            if (_elapsed >= _das)
            {
                // TODO: Apply ARR

                var moves = GetMovesPerFrame(dt);

                for (int i = 0; i < moves; i++)
                {
                    if (_isLeftDown)
                    {
                        Board.PieceMoveLeft();        
                    }

                    if (_isRightDown) // else if?
                    {
                        Board.PieceMoveRight();
                    }
                }
            }

            /*if (_moveLeft.IsPressed || _moveLeft.IsRepeating)
                Board.MoveLeft();

            if (_moveRight.IsPressed || _moveRight.IsRepeating)
                Board.MoveRight();*/

            /*var dir = _moveInput.Value;
            if (dir != 0)
                Board.Move(dir);*/

            if (Input.IsKeyDown(_controls.SoftDrop))
            {
                Board.SoftDrop(_sdf);
            }

            if (Input.IsKeyPressed(_controls.RotateClockwise))
            {
                Board.Rotate(Rotation.Clockwise);
            }
            if (Input.IsKeyPressed(_controls.RotateCounterClockwise))
            {
                Board.Rotate(Rotation.CounterClockwise);
            }
            if (Input.IsKeyPressed(_controls.Rotate180Deg))
            {
                Board.Rotate(Rotation.Deg180);
            }

            if (Input.IsKeyPressed(_controls.Hold))
            {
                _heldPiece?.HoldPiece();
            }

            if (Input.IsKeyPressed(_controls.Restart))
            {
                Board.Reset(); // TODO: Restart correctly
                _restartAction.Invoke();
            }
        }

        private int GetMovesPerFrame(float dt)
        {
            if (_arr == 0)
                return 10;

            var fps = 60f;
            var sec = 1f / fps; // ~0.0167
            var ms = sec * 1000; // ~16.7

            var dtDiff = dt - sec;
            
            // Test: 0, 1, 2, 3, 4, etc.


            return 1;
        }
    }
}