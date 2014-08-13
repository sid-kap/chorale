# Chorale Builder

Haskell + Lilypond chorale interpreter. Converts chord progression into 4-part Bach-style chorale sheet music + midi audio.

# Dependencies

For Ubuntu/Debian users:
```bash
sudo apt-get install haskell
sudo apt-get install lilypond
```

# Input Format

The program reads in an input of a chord progression.
The progression should be saved to ``` progression.txt ```

Format Rules
* Write chords in roman numeral format.
* Use uppercase for major, lowercase for minor. (Only major and minor chords supported at this time.)
* Use b or # for flat or sharp chord root.
* Separate chords with spaces.

Example
```
I V vi iii IV I IV V
```


# Example run

```bash
# cd git root
echo "I V vi iii IV I IV V" >> progression.txt
chmod +x chorale.sh
./chorale.sh
cd output
ls       # you should see the outputted files
```
