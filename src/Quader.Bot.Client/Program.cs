using System.Diagnostics;
using Nez.Persistence;
using Quader.Bot.Api;
using Quader.Bot.Api.Requests;
using ZetaIpc.Runtime.Server;
using BotMove = Quader.Bot.Api.BotMove;
using ColdClear = Quader.Bot.Api.Bots.ColdClear;

namespace Quader.Bot.Client
{
    internal class Program
    {
        private static IBot _coldClearBot = null!;
        private static bool _shouldClose = false;
        private static bool _noRequest = false;
        private static Stopwatch _stopwatch;

        private static readonly uint _maxWaitTimeTicks = 10_000;

        static void Main(string[] args)
        {
            //if (args.Length == 0)
            //    throw new Exception("No arguments provided");

            Console.WriteLine("Starting IPC Server");

            IpcServer server = new IpcServer();
            //IpcClient client = new IpcClient();

            server.Start(10077);
            //client.Initialize(10078);

            Console.WriteLine("IPC Server Started Successfully");

            _stopwatch = new Stopwatch();

            _coldClearBot = new ColdClear();

            server.ReceivedRequest += (sender, eventArgs) =>
            {
                if (eventArgs.Request == "stop")
                {
                    Console.WriteLine("Stopping IPC Server");
                    server.Stop();
                }

                var response = HandleRequest(eventArgs.Request);
                eventArgs.Response = Json.ToJson(response);
                eventArgs.Handled = true;
            };

            _stopwatch.Start();

            while (!_shouldClose)
            {
                if (_stopwatch.ElapsedMilliseconds >= _maxWaitTimeTicks)
                    break;
            }

            _coldClearBot.Destroy();
            server.Stop();

            Console.WriteLine("IPC Server Stopped");
        }

        private static object HandleRequest(string request)
        {
            if (request == "stop")
            {
                _shouldClose = true;
                return new IpcResponse { Message = "OK" };
            }

            _stopwatch.Restart();

            var obj = Json.FromJson<IpcRequest>(request);

            if (obj == null)
                throw new Exception("Invalid Request: " + request);

            var command = obj.Command;

            if (command == "sync")
            {
                return new IpcResponse
                {
                    Message = "sync success"
                };
            }
            
            if (command == "start")
            {
                string message;

                try
                {
                    var startData = Json.FromJson<StartRequestData>(obj.Data);
                    _coldClearBot.Start(startData.Queue, startData.RunMode, startData.Options, startData.Weights);

                    message = "OK";
                }
                catch (Exception e)
                {
                    message = e.Message;
                }

                return new IpcResponse
                {
                    Message = message
                };
            }

            if (command == "reset")
            {
                string message;

                try
                {
                    var resetData = Json.FromJson<ResetRequestData>(obj.Data);
                    _coldClearBot.Reset(resetData.Board, resetData.Combo, resetData.Back2Back);

                    message = "OK";
                }
                catch (Exception e)
                {
                    message = e.Message;
                }

                return new IpcResponse
                {
                    Message = message
                };
            }
            
            if (command == "move")
            {
                string message;
                BotMove? move = null;

                try
                {
                    var doMoveData = Json.FromJson<MoveRequestData>(obj.Data);

                    move = _coldClearBot.DoMove(doMoveData.IncomingGarbage);

                    message = "OK";
                }
                catch (Exception e)
                {
                    message = e.Message;
                }

                return new IpcResponse
                {
                    Message = message,
                    Data = Json.ToJson(new BotMovementRequestData()
                    {
                        IsHoldUsed = move?.IsHoldUsed ?? false,
                        Movements = move?.Movements,
                        MovementCount = move?.MovementCount ?? 0,
                        Status = move?.Status ?? BotStatus.Waiting
                    })
                };
            }
            
            if (command == "destroy")
            {
                string message;

                try
                {
                    _coldClearBot.Destroy();
                    message = "OK";
                }
                catch (Exception e)
                {
                    message = e.Message;
                }

                return new IpcResponse
                {
                    Message = message
                };
            }

            if (command == "push")
            {
                string message;

                try
                {
                    var pushPieceData = Json.FromJson<PushPieceRequestData>(obj.Data);

                    _coldClearBot.PushPiece(pushPieceData.Piece);

                    message = "OK";
                }
                catch (Exception e)
                {
                    message = e.Message;
                }

                return new IpcResponse
                {
                    Message = message
                };
            }

            return new IpcResponse
            {
                Message = "Invalid Command: " + command
            };
        }
    }
}