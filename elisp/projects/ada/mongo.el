(defconst mongo-users '("afee0769-58ab-4d7b-8f83-e71d2a85c058"
			"4639c3e5-a593-4346-a234-c02ec601818c"
			"9a0dedc1-fcd4-4e69-abbb-62fd51906bd6"
			"67e43909-3916-48b9-b37c-e28680437994"
			"ed8aa38f-0995-4780-9720-ce4c68b01f0b"
			"133c023c-7cf1-49b2-9033-11da66b0d1a8"
			"155bc004-3a79-49e8-8173-76aabb056cd6"
			"54928ad0-1c00-4c5f-b443-ce5630ef229e"
			"057536b5-0c81-4e48-9daf-92c370b314c1"
			"55cfddf4-56f1-422b-a7ca-818859057999"
			"daddc2fc-7683-4397-bcf5-35b32624ef96"
			"d4f77240-3303-4ad2-af46-6fd92728a05b"
			"9581ede0-301d-4e35-9b99-c6aef53f7652"
			"ab0beafc-cf77-4fba-9251-8093231c1575"
			"32bd289e-0413-47b0-b55c-a74e397096c5"
			"f26599e4-0d39-438e-aff1-1e9c0799e558"
			"e788da9a-06db-480e-9a66-a637f3d1f52d"
			"30fded7f-78e9-4045-9c15-11335ab566fc"
			"b660ee23-b24e-418e-b02e-0db7b5b89efa"
			"615f07a0-21f2-4de0-9d15-958fa446d71b"
			"c6cf8676-571a-4d88-a6b6-c72992c632ab"
			"0044d962-217a-42e2-b9a4-5e3a041b9e7f"))

(defconst mongo-components '("2d9b0499-3bec-3757-a33b-1e99cc1c1881"
			     "88a3d960-dc83-3297-b1ec-1a79f52e7f4a"
			     "c29211b0-2aaf-3cd6-bce3-27d7ccbebdea"
			     "c3477b68-c4ed-3629-8092-3180e771cbf1"))

(defun ada-mongo-generate-user-component-id (user component)
  (format "{\"userPseudonym\": \"%s\", \"componentUuid\": \"%s\"}"
    user component))
;;(ada-mongo-generate-user-component-id "qwe" "ewq")

(defun ada-mongo-generate-user-component-ids (users components)
  (concat* (vxw users components #'ada-mongo-generate-user-component-id)
    :in ", "))
;;(ada-mongo-generate-user-component-ids mongo-users mongo-components)

(defun ada-mongo-generate-status-query (users components)
  (concat* (vxw users components #'ada-mongo-generate-user-component-id)
    :in ", "))
db['state-user-component-170'].find({"_id": { $in: [{"userPseudonym": ..., "componentUuid": ...}, ...]}})
(provide 'mongo)
