(require 'ada-component)

(defconst +ada-problem-books-csv-string+
  "A life in the sky; https://editor.cdu.no/admin-web/component/5e6270f36be4710017dc2a11
Animal tricks; https://editor.cdu.no/admin-web/component/5e624234dcd87c001e787d9b
Beaks and Feet; https://editor.cdu.no/admin-web/component/5e625d56dcd87c001e787ed1
Big Animal vet; https://editor.cdu.no/admin-web/component/5e6238786be4710017dc27ae
Big Ears and sticky fingers; https://editor.cdu.no/admin-web/component/5e626e83b72b00001796515f
Bug Buzz!; https://editor.cdu.no/admin-web/component/5e626613d600700025a945bc
Can Fish Fly?; https://editor.cdu.no/admin-web/component/5e624386dcd87c001e787da7
Colour codes; https://editor.cdu.no/admin-web/component/5e626c61dcd87c001e787f27
Deep down weird; https://editor.cdu.no/admin-web/component/5e62650cdcd87c001e787f19
Dive! Dive!; https://editor.cdu.no/admin-web/component/5e623d79eea129001ef9e1fb
Fantastic plants and animals; https://editor.cdu.no/admin-web/component/5e6251331b399a007c9c7616
Flight or Fright?; https://editor.cdu.no/admin-web/component/5e626f6bb72b000017965167
Floppy and the bone; https://editor.cdu.no/admin-web/component/5e6224e6b72b000017964ff5
Flying kicks; https://editor.cdu.no/admin-web/component/5e6263a397b0c8002519760c
History's marvellous Mistakes; https://editor.cdu.no/admin-web/component/5e62674cd600700025a945c2
How can I help you?; https://editor.cdu.no/admin-web/component/5e624e5097b0c800251974be
How we see; https://editor.cdu.no/admin-web/component/5e62412f1b399a007c9c7593
Husky Adventure; https://editor.cdu.no/admin-web/component/5e6245536c5ccd0010b3ab4f
I can trick a tiger; https://editor.cdu.no/admin-web/component/5e62489d97b0c800251974a7
Legs!; https://editor.cdu.no/admin-web/component/5e6232bbeea129001ef9e1ed
Lemon; https://editor.cdu.no/admin-web/component/5e6234ef6c5ccd0010b3ab12
Let's make comics!; https://editor.cdu.no/admin-web/component/5e627830eea129001ef9e3f8
Look smart; https://editor.cdu.no/admin-web/component/5e623215dcd87c001e787d16
Mini Marvels; https://editor.cdu.no/admin-web/component/5e6275db6c5ccd0010b3abf1
Mud, metal and logs; https://editor.cdu.no/admin-web/component/5e624954b72b000017965051
Off to the Beach; https://editor.cdu.no/admin-web/component/5e623bda6be4710017dc27bb
On the sand; https://editor.cdu.no/admin-web/component/5e6247ec1b399a007c9c75f4
One potato, two potatoes; https://editor.cdu.no/admin-web/component/5e625223eea129001ef9e282
Our class tiger; https://editor.cdu.no/admin-web/component/5e625b5b6be4710017dc28c4
Our Siberian Journey; https://editor.cdu.no/admin-web/component/5e627c28d600700025a945d8
Outdoor Art; https://editor.cdu.no/admin-web/component/5e627e6d97b0c800251976d5
Pancakes; https://editor.cdu.no/admin-web/component/5e624b5ceea129001ef9e238
Perfect Pets; https://editor.cdu.no/admin-web/component/5e6258981b399a007c9c7630
Pick your Queen!; https://editor.cdu.no/admin-web/component/5e6260916be4710017dc28d9
Pirate Adventure; https://editor.cdu.no/admin-web/component/5e624648b72b00001796502d
Real Heroes; https://editor.cdu.no/admin-web/component/5e6281bf1b399a007c9c784e
Robot Zoo; https://editor.cdu.no/admin-web/component/5e62535f6c5ccd0010b3abbb
Scratch's Bad Reputation; https://editor.cdu.no/admin-web/component/5e627301d600700025a945cd
Silly Races; https://editor.cdu.no/admin-web/component/5e6226d7d600700025a94357
Skills and thrills; https://editor.cdu.no/admin-web/component/5e62793e6be4710017dc2a19
Snack attack; https://editor.cdu.no/admin-web/component/5e624f4ceea129001ef9e24d
Space dad; https://editor.cdu.no/admin-web/component/5e624cb797b0c800251974b8
Spread the word; https://editor.cdu.no/admin-web/component/5e6262186be4710017dc2904
Stuck in the Mud; https://editor.cdu.no/admin-web/component/5e622a01dcd87c001e787be4
Tasty Travels; https://editor.cdu.no/admin-web/component/5e627409dcd87c001e787f37
The Dinosaur hunters; https://editor.cdu.no/admin-web/component/5e6271d5b72b00001796516d
The Life of Leonardo; https://editor.cdu.no/admin-web/component/5e627a80b72b000017965177
The scarf; https://editor.cdu.no/admin-web/component/5e622d6fdcd87c001e787bed
The snowman; https://editor.cdu.no/admin-web/component/5e6226321b399a007c9c7580
The Storm; https://editor.cdu.no/admin-web/component/5e622aeb6c5ccd0010b3aa83
The Toy's party; https://editor.cdu.no/admin-web/component/5e622cb0eea129001ef9e1d5
Things with wings; https://editor.cdu.no/admin-web/component/5e62374a6c5ccd0010b3ab1c
Tools and Animals; https://editor.cdu.no/admin-web/component/5e6233fa6be4710017dc279f
Village in the Snow; https://editor.cdu.no/admin-web/component/5e6247046c5ccd0010b3ab6d
Way-out Day-Out; https://editor.cdu.no/admin-web/component/5e62692beea129001ef9e3ab
Who eaths who?; https://editor.cdu.no/admin-web/component/5e625a62eea129001ef9e2fb
Wild Wheels; https://editor.cdu.no/admin-web/component/5e625c7aeea129001ef9e325
Your body; https://editor.cdu.no/admin-web/component/5e626d52dcd87c001e787f2d
Zoom in; https://editor.cdu.no/admin-web/component/5e62502a1b399a007c9c7609
Zoom out; https://editor.cdu.no/admin-web/component/5e6259821b399a007c9c7639")

(cl-defun ada-problem-books (&optional (string +ada-problem-books-csv-string+))
  (parse-csv-string string))
;;(ada-problem-books +ada-problem-books-csv-string+)

(defun component-ids-to-source-id-string (ids)
  (concat* (component ids :source-id)
    :in "\n"
    :key #'first))

(defun find-folders-to-republish (editor-url gateway-ids)
  (cl-intersection
   gateway-ids
   (component-parent-ids
   (latest-component-id-from-source-id
    (last-elt (split-string editor-url "/"))))))
;;(find-folders-to-republish "https://editor.cdu.no/admin-web/component/5e6238786be4710017dc27ae" qwe) nil

(defun report-folders-to-republish ()
  (let ((gateway-component-ids qwe))
    (cl-loop for (title url) in (ada-problem-books)
	     if (find-folders-to-republish url gateway-component-ids)
	     collect (format "%s\n%s\n%s"
		       title
		       (string-trim url)
		       (component-ids-to-source-id-string it)))))
;;(setf result (report-folders-to-republish))

(concat* result :in "\n\n")
(provide 'find-folders-to-republish)
