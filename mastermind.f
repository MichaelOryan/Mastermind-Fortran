c     Mastermind
c     CSCI337 - Assignment One - Fortran
c     Michael O'Ryan - 4612450
c
c     Compatiable with gfortran under Ubuntu 14.04
c
c     Algorithm for computer solving players code notes
c     Set of all possible codes are stored in S
c     Matrix has whether a code is still possible from previous guesses
c     After a guess is made Matrix is updated as to whether a code
c     Is still valid
c
c     S is then partitioned using the updated matrix into
c     positions towards the left (towards 1) for those still
c     possible and positions towards the right (towards size of S) for
c     those no longer possible based on previous guesses
c
c     Guesses are made by selecting at random a still possible code
c
c     Codes are checked by the same method as a user would
c     The guessed code is checked against the answer code to see
c     how many colours and positions match
c
c     To determine whether a code is possible. Two codes are checked
c     against one another. The code the computer guessed is considered
c     the answer code and the other in S is a "guess"
c     If the colours and positions counts are the same as the computer
c     received for the answer code, ie; it's actual guess
c     then the "guess" is a possible code that the player has guessed
c     Computer guesses RGBB
c     Player enters 1 position, 1 colour matches
c     BGYY has a 1 position and 1 colour match to RGBB
c     Thus BGYY is a possible match because it's position and colour
c     match count are the same as those produced by RGBB from the
c     player
c     RGYY is then tried and has 2 position matches and 0 colours
c     to RGBB. Thus RGYY is not a possible code because it produced a
c     different colour and position match count than RGBB
c
c     For each guess the computer makes a random code from S that is
c     still a possible code
c
c     Sources
c     D E Knuth (1976-77), The computer as master mind, viewed 17/3/2017
c     ,<http://www.cs.uni.edu/~wallingf/teaching/cs3530/resources/knuth-
c     mastermind.pdf>
c
c     Weisstein, Eric W. "Mastermind." From MathWorld--A Wolfram Web
c     Resource, viewed 17/3/2017,
c     <http://mathworld.wolfram.com/Mastermind.html>
c
c     Mastermind (board game), viewed 17/3/2017,
c     <https://en.wikipedia.org/wiki/Mastermind_(board_game)>

      program master
        implicit none
        character ch ! character from input
        logical again ! Another round
c       For random number gen
        integer*4 tArray(3)
        real n
        integer csz, psz !Number of colours and positions for round

c       Initalise values
        csz = 4
        psz = 4

        call itime(tArray)
        n = rand ( tArray(1)+tArray(2)+tArray(3) )

c       Set no colour for console
        call clrrem()

c       Do intro text
        call intro()

c       Initialise game loop vars
        ch = ' '
        again = .true.

c       Game loop
        do while(again)
          ch = ' ' ! Reset user character input
c         Get number of colours and positions from user

          call getsz(csz, psz)

c         Print available colours
          call prntclrs (csz)

c         Who picks the code? User or computer
10        write (*,'(a)'), "Who do you want to pick the code?"
          write (*,'(a)'),"(y)ou"
          write (*,'(a)'),"(c)omputer"

          read (*, '(a1)', err=10), ch
          do while(.not.(either(ch,'y','Y').or.either(ch,'c','C')))
20          write (*,'(a$)'),"Please enter (y) for you "
            write(*, '(a)'),"or (c) for the computer"
            read (*, '(a1)', err=20), ch
          end do


c         Go to computer guessing or user guessing games
          if (either(ch,'y','Y')) then
            call cmpgus(csz, psz) ! Computer Guesses
          else
            call plrgus(csz, psz) ! User guesses
          end if

c         Reset user input
          ch = ' '

c         Ask if user wants to play again
          do while(.not.(either(ch,'y','Y').or.either(ch,'n','N')))
30          write (*,'(a)'),"Play again? (y)es (n)o"
            read (*, '(a1)', err=30),ch
          end do

          if(either(ch,'n','N')) then
            again = .false.
          end if
        end do

      contains

c       Returns if either ch1 or ch2 is equal to tar
      logical function either(tar, ch1, ch2) result(res)
        character tar, ch1, ch2
        res = ch1.eq.tar.or.ch2.eq.tar
      end function either

c     Return a random integer from 1 to last
      integer function random(last) result(res)
        implicit none
        integer last
        res = rand(0) * last + 1
      end function random

c     Player guesses computer code game
c     csz number of colours
c     psz number of positions
c     g array of user guess
c     ans computers hidden code user has to guess
c     c number of colours user got correct
c     p number of positions user got correct
c     cnt number of guesses taken
      subroutine plrgus(csz, psz)
        implicit none
        integer csz, psz
        integer g(psz), ans(psz), c, p, i
        integer cnt

c       set number of guesses taken to 0
        cnt = 0

c       Generate computers code
        do i = 1,psz
          ans(i) = random(csz)
        end do

        c = 0
        p = 0

c       While user hasn't matched all positions they keep guessing
        do while (.not.p.eq.psz)

c         Get user guess as g
          call gtgues(g, csz, psz)

c         Write colours picked by user for their benefit
          call wrtclr(g(1))
          do i=2,psz
            write (*, '(a$)'), ", "
            if(i.eq.7) then
              write(*, '()')
            end if
            call wrtclr(g(i))
          end do

c         End line
          write(*, '()')

c         Check user guess against answer
c         c number of correct colours, p number of correct positions
          call chkgus(ans, g, c, p, csz, psz)
          call prtcrt (c, p)
c         Increment guesses taken
          cnt = cnt + 1
        end do
c       User wins, do win
        call win(cnt)

      end subroutine plrgus


c     Initalise for computer guessing users code
c     matrix true/false for whether a code is possible
c     Partitioned Set of possible codes
c     inS number of codes in S that are possible codes
c     sz size of S, number of all possible codes
      subroutine init(matrix, S, sz, inS, csz, psz)
        implicit none
        logical matrix(csz**psz)
        integer s(csz**psz), csz, psz
        integer i, sz, inS

c       Number of possible codes
        sz = csz**psz
        inS = sz

c       Starting state of S and matrix
        do i=1,sz
          s(i) = i
          matrix(i) = .true.
        end do
      end subroutine init

c     Computer guesses user code game
c     matrix true/false for whether a code is possible
c     Partitioned Set of possible codes
c     inS number of codes in S that are possible codes
c     sz size of S, number of all possible codes
c     numgus number of guesses computer has taken
c     t index of guess in S
c     g guess array
c     guess guess as an index in matrix and integer
      subroutine cmpgus(csz, psz)
        implicit none
        logical matrix(csz**psz) !(c1, c2, c3, c4) T inS F notinS
        integer S(csz**psz) !index guess(4)
        integer csz, psz ! number of colours and positions
        integer c, p ! Colour matches and position matches
        integer sz, inS !size of inS + noinS excluding prev guesses
        integer t !current guess target index in S
        integer numgus, i !Number of guesses taken
        integer g(psz), guess

c       Initalise matrix, S, sz and inS given csz and psz
        call init(matrix, S, sz, inS, csz, psz)
        numgus = 0
        p = 0

c       While computer hasn't won and more possible codes
c       Yes this means the computer knows if you cheat
        do while(.not.p.eq.psz.and.inS.gt.0)
          numgus = numgus + 1

c         Pick a random index of a possible code in S
c         1 to inS will be possible codes
c         inS + 1 to sz are not possible codes
          t = random(inS) ! index of guess in S
          guess = s(t) ! integer representing code to guess

          write(*,'(a$)') "Here's my guess. "

c         convert guess (integer) to g (array)
          call itog(guess, g, csz, psz)

c         Write guess to console for user to check in appropriate colour
          call wrtclr(g(1))
          do i=2,psz
            write (*, '(a$)'), ", "
            call wrtclr(g(i))
          end do
          write(*, '(a)'), "."

c         Prompt for correct colours and positions from user
10        write (*, '(/a$)'), "How many positions "
          write (*, '(a$)'), "and colours did I get correct? "
c         p positions, c colours
          read (*, *, err=10), p, c

c       while user input is not possible
        do while(p.gt.psz.or.p.lt.0.or.c.gt.csz.or.c.lt.0.or.c+p.gt.psz)
20          write (*,'(a)'),"Invalid combination!"
            write (*, '(a$)'), "How many positions "
            write (*, '(a$)'), "and colours did I get correct? "
            read (*, *,err=20), p, c
        end do

c         If computer hasn't won
          if(p.lt.psz) then
c           Update whether guesses are possible in matrix
            call updMtx(matrix, S, guess, inS, c, p, csz, psz)
c           Remove this guess from possible codes
            call remG(S, t, sz, inS)
c           Now that matrix has been updated
c           Partition S into possible and not possible codes
            inS = partS(S, matrix, inS, csz, psz) - 1
          end if

        end do

c       If computer ended up with no possible guesses
c       User cheated, let them know the computer knows
        if(p.lt.psz) then
          write (*,'(a$)'),"I think you "
          call clrred()
          write(*, '(a$)'), "cheated!"
          call clrrem()
          write(*, '(a)'), " :("

c       Otherwise user wins. Congratulate them :D
        else
          write (*,'(a$)'),"I guessed your combo in"
          write (*,'(i2$)'), numgus
          write (*, '(a$)'), " guesses"(1:6+plural(numgus)*2)
          write (*,'(a)'), "!"
        end if

      end subroutine cmpgus

c     Return a guess array as an integer index
c     g array of guess colours
c     csz number of colours in game
c     psz number of colours in game
      integer function gtoi(g, csz, psz) result(i)
        implicit none
        integer g(psz), csz, psz, h

c       Base csz to base 10
c       First index doesn't change
        i = g(psz)

c       Each index adds (x-1) * base ^ position
        do h= 1, psz-1
          i = i + (g(psz-h)-1) * csz**h
        end do


      end function gtoi

c     convert an index to a guess array of base csz
c     g array of guess colours
c     csz number of colours in game
c     psz number of colours in game
c     i integer representation of code in base 10

      subroutine itog(i, g, csz, psz)
        implicit none
        integer g(psz), i, it, csz, j, psz
c       Alignment for 1 to sz array rather than 0 to sz - 1
        it = i - 1

c       Each base csz position is
c       ((index / (base ^ (last position - position)) mod base) + 1
c       +1 for 1 to sz array
        do j = 1,psz
          g(j) = mod(it / (csz**(psz-j)), csz) + 1
c         if position is 0 it should be base since 1 to base
c         not 0 to base - 1
          if(g(j).lt. 1) g(j) = csz
        end do

      end subroutine itog

      !swap the guess at S(a) with S(b)
      subroutine swapS(S, ia, ib)
        implicit none
        integer S(*), ia, ib, tmp
          tmp = S(ia)
          S(ia) = S(ib)
          S(ib) = tmp
      end subroutine

c     Partitions S into possible guess and impossible ones
c     Returns the first impossible guess
c     sz is size of S
      integer function partS(S, matrix, sz, csz, psz) result(fnS)
        implicit none
        integer S(csz**psz), sz, csz, psz
        logical matrix(csz**psz)
        integer g
        integer f, b !front back iters
        f = 1
        b = sz
c       While our iterators haven't passed each other
        do while(b .ge. f)
c         Get integer representing guess at f
          g = S(f)
c         If an impossible guess, move to back (b)
          if(.not.matrix(g)) then
            call swapS(S, f, b)
c           decrement back interator to next unchecked index
            b = b - 1
          else
c           Move front iter to next index if s(f) is a possible guess
            f = f + 1
          end if
        end do
c       First impossible guess is now at f
        fnS = f
      end function partS

c     ans guess we know how many colours and positions match
c     poss possible guess we are checking if it can have the same
c     number of colour and position matches
c     c number of colour matches ans has
c     p number of position matches ans has

      logical function isMatch(ans, poss, c, p, csz, psz) result (valid)
        implicit none
        integer ans(psz), poss(psz), c, p, csz, psz
        integer cCount, pCount
        cCount = 0 ! number of colours matched between ans and poss
        pCount = 0 ! number of positions matched between ans and poss

c       Find number of colours and positions matching between
c       ans and poss
        call chkgus(poss, ans, cCount, pCount, csz, psz)

c       If the number of matching colours and positions are the same
c       as the passed in matches for ans, then poss is a possible code
        valid = (cCount .eq. c) .and. (pCount .eq. p)
      end function isMatch


c     ans answer guess
c     possible guess
c     Order probably isn't important
c     c returns number of colour matches between ans and g
c     p returns number of position matches between ans and g
c     csz possible colours in game
c     psz possible positions in game
      subroutine chkgus(g, ans, c, p, csz, psz)
        implicit none
        integer csz, psz, g(psz), ans(psz), c, p
        integer colours(csz)
        integer cCount, pCount, i

c       Match count to 0
        cCount = 0
        pCount = 0

c       Init colour difference array, ans adds to, g takes away
        do i=1, csz
          colours(i) = 0
        end do

c       For each position
        do i=1,psz

c         Take ith positions colour from g away from colour array
c         If the matching index in colour is positive then there was
c         a colour match
          if(colours(g(i)).gt.0) then
            cCount = cCount + 1
          end if
          colours(g(i)) = colours(g(i)) - 1

c         Grab the ith positions colour from ans and add to colour array
c         If the matching index in colour is negative then there was
c         a colour match
          if(colours(ans(i)).lt.0) then
            cCount = cCount + 1
          end if
          colours(ans(i)) = colours(ans(i)) + 1

c         If the ith position has the same colour in ans and g then
c         there was a position match
          if(g(i).eq.ans(i)) then
            pCount = pCount + 1
          end if
        end do

c       Take position matches away from colour matches
c       Since position matches have the same colour and we only
c       want colours not matching positions
        c = cCount - pCount
        p = pCount
      end subroutine chkgus


c     Update the matrix whether codes are possible
c     Matrix whether a code is possible or not
c     S set of codes partitioned by possible and not possible
c     guess guess taken
c     c number of colour matches for guess
c     p number of position matches for guess
c     index of last possible code in S
c     csz numer of colours in game
c     psz numer of positions in game

      subroutine updMtx(matrix, S, guess, inS, c, p, csz, psz)
        implicit none
        logical matrix(csz**psz), tf
        integer S(csz**psz), guess, inS, i, c, p, t, csz, psz
        integer ans(psz), poss(psz)
c       covert guess as an integer to a code array
        call itog(guess, ans, csz, psz)

c       For each possible code check if it is still possible
        do i=1,inS
c         get next possible guess as integer code
          t = S(i)
c         convert to an array (poss)
          call itog(t, poss, csz, psz)
c         See if it is a possible code given the guess and matches
          tf = isMatch(ans, poss, c, p, csz, psz)
c         Update matrix to whether code is still possible
          matrix(t) = tf
          end do
      end subroutine updMtx

c     Remove an index in S from possible codes
      subroutine remG(S, g, sz, inS)
        implicit none
        integer S(*), g, sz, inS

c       Swap with last possible code in S
        if(g.le.inS) then
          call swapS(S, g, inS)
          g = inS
          inS = inS - 1
        end if

c       Swap with back of S
c       Remove as a guess not taken
c       S -> Possible guesses | not possible guesses | taken guesses
c       Useful for algorithm which needs S partitioned that way
        call swapS(S, g, sz)
        sz = sz - 1

      end subroutine remG

c     User interaction

c     num number of guesses taken
      subroutine win(num)
        implicit none
        integer num
        write(*, '(a$)'), "You guessed it in "
        write(*, '(i0, a, a)'), num, " goes"(1:3+plural(num)*2), "."

      end subroutine win

c     Print number of correct colours and positions user guessed
c     of computers code
      subroutine prtcrt (c, p)
        implicit none
        integer c, p
        write (*, "(a, i0$)"), "Your guess contains ", p
        write (*, "(a$)") , " pieces"(1:6 + plural(p))
        write (*, "(a)") , " in the right place"

        write (*, "(a, i0, a$)"), "and ", c, " pieces"(1:6+plural(c))
        write (*, "(a)"), " of the correct colour in the wrong"
        write (*, "(a)"), "place."
      end subroutine prtcrt

c     Write a colour to console as it's colour
c     red in colour red, blue in colour blue, etc
      subroutine wrtclr(i)
        integer i

        if(i.eq.1) then
          call clrred()
          write (*, '(a$)'), "Red"
          call clrrem()
        end if

        if(i.eq.2) then
          call clrpur()
          write (*, '(a$)'), "Purple"
          call clrrem()
        end if

        if(i.eq.3) then
          call clrblu()
          write (*, '(a$)'), "Blue"
          call clrrem()
        end if

        if(i.eq.4) then
          call clrgrn()
          write (*, '(a$)'), "Green"
          call clrrem()
        end if

        if(i.eq.5) then
          call clryel()
          write (*, '(a$)'), "Yellow"
          call clrrem()
        end if

        if(i.eq.6) then
          call clrgry()
          write (*, '(a$)'), "Grey"
          call clrrem()
        end if

        if(i.eq.7) then
          call clraqa()
          write (*, '(a$)'), "Aqua"
          call clrrem()
        end if

        if(i.eq.8) then
          call clrpnk()
          write (*, '(a$)'), "Pink"
          call clrrem()
        end if
      end subroutine

c     Remove colour setting to default colour
      subroutine clrrem()
        write(*, '(a$)'), achar(27) // '[0m'
      end subroutine

c     Set console to print in colour red
      subroutine clrred()
        write(*, '(a$)'), achar(27)// '[31m'
      end subroutine

c     Set console to print in colour yellow
      subroutine clryel()
        write(*, '(a$)'), achar(27) // '[93m'
      end subroutine

c     Set console to print in colour blue
      subroutine clrblu()
        write(*, '(a$)'), achar(27) // '[34m'
      end subroutine

c     Set console to print in colour purple
      subroutine clrpur()
        write(*, '(a$)'), achar(27) //  '[35m'
      end subroutine

c     Set console to print in colour green
      subroutine clrgrn()
        write(*, '(a$)'), achar(27) //  '[32m'
      end subroutine

c     Set console to print in colour pnk
      subroutine clrpnk()
        write(*, '(a$)'), achar(27) //  '[95m'
      end subroutine

c     Set console to print in colour aqua
      subroutine clraqa()
        write(*, '(a$)'), achar(27) //  '[36m'
      end subroutine

c     Set console to print in colour grey
      subroutine clrgry()
        write(*, '(a$)'), achar(27) //  '[90m'
      end subroutine

c     Print text for possible colours n in game
c     Ie; you can use this colour, and this one, and this one, etc
      subroutine prntclrs (n)
        implicit none
        integer n, cnt
        cnt = n

        write(*, '(a$)'), "Colours are "

        if(cnt.gt.0) then
          call clrred()
          write (*, '(a$)'), "(R)ed"
          call clrrem()
          call prntac(cnt)
          cnt = cnt - 1
        end if

        if(cnt.gt.0) then
          call clrpur()
          write (*, '(a$)'), "(P)urple"
          call clrrem()
          call prntac(cnt)
          cnt = cnt - 1
        end if

        if(cnt.gt.0) then
          call clrblu()
          write (*, '(a$)'), "(B)lue"
          call clrrem()
          call prntac(cnt)
          cnt = cnt - 1
        end if

        if(cnt.gt.0) then
          call clrgrn()
          write (*, '(/a$)'), "(G)reen"
          call clrrem()
          call prntac(cnt)
          cnt = cnt - 1
        end if

        if(cnt.gt.0) then
          call clryel()
          write (*, '(a$)'), "(Y)ellow"
          call clrrem()
          call prntac(cnt)
          cnt = cnt - 1
        end if

        if(cnt.gt.0) then
          call clrgry()
          write (*, '(a$)'), "Gr(e)y"
          call clrrem()
          call prntac(cnt)
          cnt = cnt - 1
        end if

        if(cnt.gt.0) then
          call clraqa()
          write (*, '(a$)'), "(A)qua"
          call clrrem()
          call prntac(cnt)
          cnt = cnt - 1
        end if

        if(cnt.gt.0) then
          call clrpnk()
          write (*, '(a$)'), "Pi(n)k"
          call clrrem()
          call prntac(cnt)
          cnt = cnt - 1
        end if


        write(*, '()')


      end subroutine prntclrs

c     Print either a comma or and depending on how many additional
c     p is number of elements after this one
      subroutine prntac(p)
        implicit none
        integer p
        if(p.gt.2) then
          write (*, '(a2$)'), ", "
        else if (p.gt.1) then
          write (*, '(a5$)'), " and "
        end if
      end subroutine prntac

c     Convert user input for a colour into an integer
c     (r)ed 1, (p)urple 2, (b)lue 3, (g)reen 4, (y)ellow 5
c     gr(e)y 6, (a)qua 7, pi(n)k 8
c     otherwise return 0 meaning invalid choice
      integer function col2n(ch) result(res)
        implicit none
        character ch
        if(ch.eq.'r'.or.ch.eq.'R') then
          res = 1
        else if (ch.eq.'p'.or.ch.eq.'P') then
          res = 2
        else if (ch.eq.'b'.or.ch.eq.'B') then
          res = 3
        else if (ch.eq.'g'.or.ch.eq.'G') then
          res = 4
        else if (ch.eq.'y'.or.ch.eq.'Y') then
          res = 5
        else if (ch.eq.'e'.or.ch.eq.'E') then
          res = 6
        else if (ch.eq.'a'.or.ch.eq.'A') then
          res = 7
        else if (ch.eq.'n'.or.ch.eq.'N') then
          res = 8
        else
          res = 0
        end if


      end function

c     print a string in a colour to console
      subroutine prtstr(clr, str, start, end)
        integer clr, start, end
        character str(*)

        if(clr.eq.1) then
          call clrred()
        else if (clr.eq.2) then
          call clrpur()
        else if (clr.eq.3) then
          call clrblu()
        else if (clr.eq.4) then
          call clrgrn()
        else if (clr.eq.5) then
          call clryel()
        else if (clr.eq.6) then
          call clrgry()
        else if (clr.eq.7) then
          call clraqa()
        else if (clr.eq.8) then
          call clrpnk()
        end if
        write(*, '(a$)'), str(start:end)
        call clrrem()

      end subroutine prtstr

c     print a character in a colour
      subroutine prtchr(clr, chr)
        integer clr
        character chr

        if(clr.eq.1) then
          call clrred()
        else if (clr.eq.2) then
          call clrpur()
        else if (clr.eq.3) then
          call clrblu()
        else if (clr.eq.4) then
          call clrgrn()
        else if (clr.eq.5) then
          call clryel()
        else if (clr.eq.6) then
          call clrgry()
        else if (clr.eq.7) then
          call clraqa()
        else if (clr.eq.8) then
          call clrpnk()
        end if
        write(*, '(a1$)'), chr
        call clrrem()

      end subroutine prtchr

c     get a guess from a user into g array
      subroutine gtgues(g, csz, psz)
        implicit none
        integer g(psz), csz, psz, i, clr
        character letter
        logical error
        error = .true.

c       While user has entered invalid input
        do while(error)


c         Write a guide for the user to show number of postions
c         and colours and letters for each colour
c                         |RPBGY|EAN <- colour codes
c         Enter your guess:RPPGG   <-Player input
10        write(*, '(a$)'), "                 |"
          do i=1, psz
            clr = mod(i, 8)
            if(clr.eq.0) clr = 8
            call prtstr(clr, "RPBGYEAN", i, i)
          end do
          write(*, '(a$)'), "|"
          do i=i,csz
            clr = mod(i, 8)
            if(clr.eq.0) clr = 8
              call prtstr(clr, "RPBGYEAN", i, i)
          end do
          write(*, '()')

c         Get guess from user, only first psz letters
          write(*, '(a$)'), "Enter your guess: "

          error = .false. !no error with input
          i = 1
          do while(.not.error.and.i.le.psz)
            letter = ' '
c           to eat or not to eat new line character
            if(i.eq.psz) then
              read(*, '(a1)', err=10), letter
            else
              read(*, '(a1$)', err=10), letter
            end if

            g(i) = col2n(letter)

c           0 means invalid input, > csz means invalid colour entered
            error = error .or. g(i).eq.0 .or. g(i).gt.csz

c           if error eat remaining characters on line and
c           restart getting user guess from the beginning
            if(error) then
              if(.not.i.eq.psz) read(*, '(a1)', err=20), letter
20            i = psz
            else
              i = i + 1
            end if
          end do


        end do

      end subroutine gtgues

c     Intro to game
      subroutine intro()
        implicit none
        character enter
        integer i, clr
        write(*, '(a$)'), "Welcome to "

c       Print Mastermind in colours of game
        do i=1, 10
          clr = mod(i, 8)
          if(clr.eq.0) clr = 8
          call prtstr(clr, "Mastermind", i, i)
        end do
c       Wait for user interaction before proceeding
        write(*, '(a$)'), ", press Enter to begin:"
        read (*, '(a1)', err=10), enter
10      continue
      end subroutine intro

c     Ask user for size of game. number of colours and positions
c     csz number of colours
c     psz number of positions
      subroutine getsz(csz, psz)
        implicit none
        integer csz, psz
10      write(*, '(a$)'), "How many pegs and colours should I use: "
        read(*, *, err=10), psz, csz

c       while psz or psz outside of 1 to 8 constraint
        do while (psz.lt.1.or.csz.lt.1.or.psz.gt.8.or.csz.gt.8)
20        write(*, '(a$)'), "Please enter two values from 1 to 8 "
          write(*, '(a$)'), "eg; 4 6: "
          read(*, *, err=20), psz, csz
        end do
      end subroutine getsz

c     return 0 when n = 1
c     returns 1 for any other number
c     0 is a plural!
      integer function plural(n)
        implicit none
        integer n
        if(n.eq.1) then
          plural = 0
        else
          plural = 1
        end if
      end function plural

      end program master
