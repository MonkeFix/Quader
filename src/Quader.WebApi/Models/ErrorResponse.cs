using Newtonsoft.Json;

namespace Quader.WebApi.Models;

public class ErrorResponse
{
    [JsonRequired]
    public string? Error { get; set; }
}