function Start-Rserve
{
    if (Get-Process Rserve -ErrorAction SilentlyContinue)
    {
        Write-Host "Rserve already running"
    }
    else
    {
         Start-Process -NoNewWindow ~/Documents/R/win-library/3.5/Rserve/libs/x64/Rserve.exe `
            -ArgumentList "--RS-workdir . --RS-port 8383 --RS-source rserve-load-data.R"
    }
}

function Stop-Rserve
{
    Stop-Process -Name Rserve -ErrorAction SilentlyContinue

    while (Get-Process Rserve -ErrorAction SilentlyContinue)
    {
        Start-Sleep 1 # cleanup wait
    }
}

function Restart-Rserve
{
    Stop-Rserve
    Start-Rserve
}

function Get-RserveStatus
{
    $rserve = Get-Process Rserve -ErrorAction SilentlyContinue
    if ($rserve)
    {
        Write-Host "Rserve already running"
    }
    else
    {
        Write-Host "Rserve is not running"
    }
}

if ($args[0] -match '^(start|stop|restart|status)$')
{
    switch ($args[0])
    {
        "start"   { Start-Rserve }
        "stop"    { Stop-Rserve }
        "restart" { Restart-Rserve }
        "status"  { Get-RserveStatus }
    }
}
else
{
    Write-Host "Usage: ./rserve.sh [start | stop | restart | status]"
}
