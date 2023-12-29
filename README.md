# autocad-lisp-routines
A collection of lip routines I use for AutoCAD.

# Usage
| Name              | Command Trigger | Description                                                                           | Notes                                                                                   |
| ----------------- | --------------- | ------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------- |
| postfix           | pfix            | Sets the last integer in one or more mtext or text objects to the value given.        | Useful for setting source panel numbers in circuit home run labels.                     |
| prefix (left-fix) | lfix            | Sets the 1st integer in one or more mtext or text objects to the value given.         | Useful for setting circuit numbers in homerun labels.                                   |
| increment         | incr            | Increments the last integer in one or more mtext or mtext objects by the value given. | Useful for updating multiple circuit numbers in homerun labels.                         |
| rotate basepoint  | rotatebase      | Rotates one or more objects around its basepoint.                                     | Used to make a rotate 90degrees keyboard shortcut. Only works for certain object types. |
| scale basepoint   | scalebase       | Scales one or more objects from its basepoint                                         | Only works for certain object types.                                                    |
| set text          | setxt           | Sets the text of one or more mtext or text objects.                                   |                                                                                         |

*Note: I used the term 'postfix' because its a short-articulate-action-word making it easy for speach recognition to match the command.*