using Microsoft.AspNetCore.Mvc;

namespace Quader.WebApi.Models;

public class ExceptionResult : IActionResult
{
    public Task ExecuteResultAsync(ActionContext context)
    {
        return Task.CompletedTask;
    }
}