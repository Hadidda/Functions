Clear-Host
Echo "Toggling F9..."  
$WShell = New-Object -com "Wscript.Shell" 
while ($true) { 
$WShell.sendkeys("{F9}") 
Start-Sleep -Milliseconds 200   
$WShell.sendkeys("{F9}") 
Start-Sleep -Seconds 120
}