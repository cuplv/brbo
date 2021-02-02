Error Box_iref::parse(BitstreamRange& range)
{
  parse_full_box_header(range);

  while (!range.eof()) {
    Reference ref;

    Error err = ref.header.parse(range);
    if (err != Error::Ok) {
      return err;
    }

    if (get_version()==0) {
      ref.from_item_ID = range.read16();
      int nRefs = range.read16();
      for (int i=0;i<nRefs;i++) {
        ref.to_item_ID.push_back( range.read16() );
        if (range.eof()) {
          break;
        }
      }
    }
    else {
      ref.from_item_ID = range.read32();
      int nRefs = range.read16();
      for (int i=0;i<nRefs;i++) {
        ref.to_item_ID.push_back( range.read32() );
        if (range.eof()) {
          break;
        }
      }
    }

    m_references.push_back(ref);
  }

  return range.get_error();
}