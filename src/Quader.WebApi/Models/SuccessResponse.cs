using Newtonsoft.Json;

namespace Quader.WebApi.Models;

public class SuccessResponse
{
    [JsonRequired] public string Message { get; set; } = "Success";
}